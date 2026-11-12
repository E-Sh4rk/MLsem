(* Handlers for the custom [mlsem/*] JSON-RPC requests backing the VS Code
   overload-merge webview. Each takes the request's [params] as raw JSON and
   returns the JSON result. They run in the engine process ([Engine]).

   Consistency model: every request carries the client's current buffer [text].
   We typecheck *that* text and resolve the request's inputs against it, so the
   client's view and the server's never drift:
   - [mlsem/overloads] resolves the binding by [position] (against the sent
     text) — used to open/refresh the panel.
   - [mlsem/mergePreview] and [mlsem/applyMerge] resolve the binding by [name]
     (stable across edits, unlike a position) and select overloads by matching
     their rendered [text]. If a requested overload text is no longer present,
     the binding changed under the client: we reply [stale] so the client
     refreshes instead of merging a subset the user did not pick.
   Ambient global config may have been mutated by an intervening typecheck, so
   we [Config.restore_all ()] before rendering/merging. *)

(* TODO: more operations for the toolkit.
   - Merge arrows: [int → int ∧ dbl → dbl] becomes [num → num] (loses
     precision) or [α ≤ num. α → α] (keeps it).
   - Split an arrow whose result union hides a dependency on the input.
   - Resolve overlap by subtracting domains, which is where negation enters;
     the user picks the policy, and the tool writes the [∖].
   - Widen an arrow to cover inputs the traces never exercised. *)

open Mlsem_app
open Yojson.Safe.Util

let decode_uri params =
  (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri

let decode_position params = params |> member "position" |> Lsp.Types.Position.t_of_yojson
let decode_name params = params |> member "name" |> to_string
let decode_overload_texts params = params |> member "overloadTexts" |> to_list |> List.map to_string

(* Apply mode: [append=true] adds the result as new [val] declaration line(s),
   keeping the existing ones; otherwise the existing declaration is replaced. *)
let mode_of params =
  match member "append" params with
  | `Bool true -> `Insert
  | _ -> `Replace

(* Optional [only]: restrict the written result to the overloads whose original
   rendered text is in this list — used by the per-overload Insert action to
   write a single overload's instantiated declaration. Absent means "all". *)
let decode_only params =
  match member "only" params with
  | `List l -> Some (List.map to_string l)
  | _ -> None

(* Merge apply: [keepOthers=true] (the scoped Apply) keeps the unselected
   overloads alongside the merged result; otherwise (the full Replace) only the
   merged declaration is written. *)
let decode_keep_others params =
  match member "keepOthers" params with
  | `Bool b -> b
  | _ -> false

(* The [instantiations] payload: per overload (identified by its rendered
   text), the variable -> concrete-type-string assignments to apply. *)
let decode_instantiations params =
  params |> member "instantiations" |> to_list
  |> List.map (fun o ->
    let overload = o |> member "overload" |> to_string in
    let assignments =
      o |> member "assignments" |> to_list
      |> List.map (fun a -> (a |> member "var" |> to_string, a |> member "type" |> to_string))
    in
    (overload, assignments) )

(* Builtin concrete types worth offering as instantiation suggestions; the
   webview also lets the user type any other type expression. *)
let builtin_type_names = ["int"; "bool"; "string"; "float"; "char"; "list"; "any"; "empty"]

(* Typecheck and cache the buffer so binding resolution below runs against
   exactly the text the request's position/name refer to. The supervisor
   fills in [text] from its copy of the document when the client omits it. *)
let sync params uri =
  match member "text" params with
  | `String text -> Store.sync_text uri text
  | _ -> ()

let not_found =
  `Assoc
    [
      ("found", `Bool false); ("name", `String ""); ("declared", `Bool false);
      ("overloads", `List []);
    ]

let error msg = `Assoc [("ok", `Bool false); ("error", `String msg)]
let stale = `Assoc [("ok", `Bool false); ("stale", `Bool true)]

(* A concrete type the user typed could not be parsed/elaborated. *)
exception Bad_type of string

(* Merge the overloads of [sigs] whose rendered text is in [wanted]. Returns the
   merged rendering together with the full list of rendered overloads (so the
   caller can keep the unselected ones). [`Stale] if any wanted text is absent
   (the overload set changed), [`Empty] if nothing was selected
   ([Signature.merge []] is ill-defined). *)
let merge_texts envs (sigs : Signature.t) (wanted : string list) =
  match wanted with
  | [] -> `Empty
  | _ -> (
      Config.restore_all () ;
      let rendered = Main.signature envs sigs in
      if not (List.for_all (fun w -> List.mem w rendered) wanted) then
        `Stale
      else
        let selected =
          List.fold_left2
            (fun acc s r -> if List.mem r wanted then s :: acc else acc)
            [] sigs rendered
          |> List.rev
        in
        match
          Main.signature envs [Signature.merge ~recursive:true (Signature.regroup selected)]
        with
        | [s] -> `Ok (s, rendered)
        | _ -> assert false (* one overload in → exactly one rendered string *) )

(* Apply [insts] to [sigs]: for each overload (matched by rendered text), apply
   its variable -> concrete-type assignments, leaving untouched overloads as-is.
   Returns the rendered result overloads (one per input overload, NOT merged),
   or [`Stale] if any requested overload text is absent (the set changed).
   [only], when present, keeps just the overloads whose *original* rendered text
   is in it (the per-overload Insert action writes a single one).
   May raise on a malformed concrete type (caught by the handler). *)
let instantiate_texts ?only envs (sigs : Signature.t) insts =
  Config.restore_all () ;
  let rendered = Main.signature envs sigs in
  if not (List.for_all (fun (t, _) -> List.mem t rendered) insts) then
    `Stale
  else
    let pairs =
      List.map2
        (fun s r ->
           let s' =
             match List.assoc_opt r insts with
             | None
             | Some [] ->
                 s
             | Some assignments ->
                 List.fold_left
                   (fun s (var, tystr) ->
                      let ty =
                        try Main.build_type envs tystr with
                        | _ -> raise (Bad_type tystr)
                      in
                      Signature.instantiate s var ty )
                   s assignments
           in
           (r, s') )
        sigs rendered
    in
    let kept =
      match only with
      | None -> pairs
      | Some texts -> List.filter (fun (r, _) -> List.mem r texts) pairs
    in
    `Ok (Main.signature envs (List.map snd kept))

(* mlsem/overloads: the binding under the cursor and its overloads, each
   rendered to [val]-compatible surface syntax (text is the overload's
   identity for selection). *)
let overloads params : Yojson.Safe.t =
  try
    let uri = decode_uri params in
    let pos = decode_position params in
    sync params uri ;
    match (Store.find_binding_at uri pos, Store.envs_of uri) with
    | Some b, Some envs ->
        Config.restore_all () ;
        let texts = Main.signature envs b.sigs in
        let strs ss = `List (List.map (fun s -> `String s) ss) in
        let overload_vars = b.sigs |> List.map (fun o -> strs (Signature.overload_vars o)) in
        let concrete_types = builtin_type_names @ Main.user_type_names envs in
        `Assoc
          [
            ("found", `Bool true); ("name", `String b.name); ("declared", `Bool b.declared);
            ("overloads", strs texts); ("overloadVars", `List overload_vars);
            ("concreteTypes", strs concrete_types);
          ]
    | _ -> not_found
  with
  | _ -> not_found

(* mlsem/mergePreview: the merged type of the selected overloads as the
   one-line [val name : ...] the apply would write. *)
let merge_preview params : Yojson.Safe.t =
  try
    let uri = decode_uri params in
    let name = decode_name params in
    let wanted = decode_overload_texts params in
    sync params uri ;
    match (Store.find_binding_by_name uri name, Store.envs_of uri) with
    | Some b, Some envs -> (
        match merge_texts envs b.sigs wanted with
        | `Empty -> error "Select at least one overload."
        | `Stale -> stale
        | `Ok (merged, _) ->
            `Assoc [("ok", `Bool true); ("text", `String ("val " ^ name ^ " : " ^ merged))] )
    | _ -> error "No binding found."
  with
  | e -> error (Printexc.to_string e)

(* mlsem/generalizePreview: the selected overloads generalized into one
   polymorphic type ([Generalize]), as the [val name : ...] line and as the
   type alone, for [mlsem/checkSignature] to check: the result claims more
   than the overloads, so it may not hold. [precision] compares it with the
   collapse of the same overloads ([Generalize.compare_with_collapse]):
   "equivalent", "more" (precise), "less" or "incomparable". *)
let generalize_preview params : Yojson.Safe.t =
  try
    let uri = decode_uri params in
    let name = decode_name params in
    let wanted = decode_overload_texts params in
    sync params uri ;
    match (Store.find_binding_by_name uri name, Store.envs_of uri) with
    | Some b, Some envs -> (
        Config.restore_all () ;
        let rendered = Main.signature envs b.sigs in
        if not (List.for_all (fun w -> List.mem w rendered) wanted) then
          stale
        else
          let pairs =
            List.combine b.sigs rendered |> List.filter (fun (_, r) -> List.mem r wanted)
          in
          match pairs with
          | [] -> error "Select at least one overload."
          | _ -> (
              let selected = List.map snd pairs in
              let g = Generalize.generalize (List.map IO.parse_type_string selected) in
              let benv, _, _, _, _ = envs in
              let overload, _ = Signature.build benv g.ty in
              (* The same collapse as [merge_texts], to compare against. *)
              let collapsed =
                Signature.merge ~recursive:true (Signature.regroup (List.map fst pairs))
              in
              let ty_of o = Signature.to_gty o |> Mlsem_types.GTy.ub in
              let precision =
                Generalize.compare_with_collapse ~copies:(List.length pairs)
                  ~generalized:(ty_of overload) ~collapsed:(ty_of collapsed)
              in
              match Main.signature envs [overload] with
              | [ty] ->
                  `Assoc
                    [
                      ("ok", `Bool true); ("text", `String ("val " ^ name ^ " : " ^ ty));
                      ("type", `String ty);
                      ("precision", `String (Generalize.precision_name precision));
                    ]
              | _ -> error "The generalized type did not print as one overload." ) )
    | _ -> error "No binding found."
  with
  | e -> error (Printexc.to_string e)

(* mlsem/applyMerge: the workspace edits that write the merged declaration —
   replacing existing [val] line(s) or inserting above the binder. Returned as
   a list for the extension to apply via [vscode.workspace.applyEdit]. *)
let apply_merge params : Yojson.Safe.t =
  try
    let uri = decode_uri params in
    let name = decode_name params in
    let wanted = decode_overload_texts params in
    sync params uri ;
    match (Store.find_binding_by_name uri name, Store.envs_of uri) with
    | Some b, Some envs -> (
        match merge_texts envs b.sigs wanted with
        | `Empty -> error "Select at least one overload."
        | `Stale -> stale
        | `Ok (merged, rendered) ->
            (* Apply (scoped) keeps the unselected overloads alongside the
               merged one; Replace (full) writes only the merged declaration;
               Insert appends the merged declaration (existing source kept). *)
            let mode = mode_of params in
            let decls =
              match mode with
              | `Insert -> [merged]
              | `Replace ->
                  if decode_keep_others params then
                    merged :: List.filter (fun r -> not (List.mem r wanted)) rendered
                  else
                    [merged]
            in
            let edits = Store.signature_edit uri b ~name ~decls ~mode in
            `Assoc
              [
                ("ok", `Bool true); ("uri", Lsp.Types.DocumentUri.yojson_of_t uri);
                ( "edits",
                  `List
                    (List.map
                       (fun (range, newText) ->
                          `Assoc
                            [
                              ("range", Lsp.Types.Range.yojson_of_t range);
                              ("newText", `String newText);
                            ] )
                       edits ) );
              ] )
    | _ -> error "No binding found."
  with
  | e -> error (Printexc.to_string e)

(* mlsem/instantiatePreview: the result overloads after applying the requested
   variable instantiations, rendered as the [val name : ...] lines apply would
   write (one per overload, kept separate). *)
let instantiate_preview params : Yojson.Safe.t =
  try
    let uri = decode_uri params in
    let name = decode_name params in
    let insts = decode_instantiations params in
    sync params uri ;
    match (Store.find_binding_by_name uri name, Store.envs_of uri) with
    | Some b, Some envs -> (
        match instantiate_texts envs b.sigs insts with
        | `Stale -> stale
        | `Ok overloads ->
            `Assoc
              [
                ("ok", `Bool true);
                ( "overloads",
                  `List (List.map (fun s -> `String ("val " ^ name ^ " : " ^ s)) overloads) );
              ] )
    | _ -> error "No binding found."
  with
  | Bad_type s -> error (Printf.sprintf "Not a valid type: %s" s)
  | e -> error (Printexc.to_string e)

(* mlsem/applyInstantiate: the workspace edits writing the instantiated
   overloads as one [val name : ...] line each. *)
let apply_instantiate params : Yojson.Safe.t =
  try
    let uri = decode_uri params in
    let name = decode_name params in
    let insts = decode_instantiations params in
    let only = decode_only params in
    sync params uri ;
    match (Store.find_binding_by_name uri name, Store.envs_of uri) with
    | Some b, Some envs -> (
        match instantiate_texts ?only envs b.sigs insts with
        | `Stale -> stale
        | `Ok overloads ->
            let edits = Store.signature_edit uri b ~name ~decls:overloads ~mode:(mode_of params) in
            `Assoc
              [
                ("ok", `Bool true); ("uri", Lsp.Types.DocumentUri.yojson_of_t uri);
                ( "edits",
                  `List
                    (List.map
                       (fun (range, newText) ->
                          `Assoc
                            [
                              ("range", Lsp.Types.Range.yojson_of_t range);
                              ("newText", `String newText);
                            ] )
                       edits ) );
              ] )
    | _ -> error "No binding found."
  with
  | Bad_type s -> error (Printf.sprintf "Not a valid type: %s" s)
  | e -> error (Printexc.to_string e)
