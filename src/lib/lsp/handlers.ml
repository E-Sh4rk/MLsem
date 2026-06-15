(* Handlers for the custom [mlsem/*] JSON-RPC requests backing the VS Code
   overload-merge webview. Each takes the request's [params] as raw JSON and
   returns the JSON result; [Server] wires them into the dispatch loop.

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

open Mlsem_app
open Yojson.Safe.Util

let decode_uri params =
  (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri

let decode_position params = params |> member "position" |> Lsp.Types.Position.t_of_yojson
let decode_name params = params |> member "name" |> to_string
let decode_overload_texts params = params |> member "overloadTexts" |> to_list |> List.map to_string

(* Typecheck and cache the client-sent buffer so binding resolution below runs
   against exactly the text the request's position/name refer to. The shipped
   client always sends [text]; if a request omits it we leave the cache as-is
   (resolution then runs against whatever was last typechecked). *)
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

(* Merge the overloads of [sigs] whose rendered text is in [wanted]. [`Stale]
   if any wanted text is absent (the overload set changed), [`Empty] if nothing
   was selected ([Signature.merge []] is ill-defined). *)
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
        match Main.signature envs [Signature.merge selected] with
        | [s] -> `Ok s
        | _ -> assert false (* one overload in → exactly one rendered string *) )

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
        `Assoc
          [
            ("found", `Bool true); ("name", `String b.name); ("declared", `Bool b.declared);
            ("overloads", `List (List.map (fun t -> `String t) texts));
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
        | `Ok merged ->
            `Assoc [("ok", `Bool true); ("text", `String ("val " ^ name ^ " : " ^ merged))] )
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
        | `Ok merged ->
            let edits = Store.merge_edit uri b ~name ~merged in
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
