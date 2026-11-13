(* The toolkit's "Extract repeated parts": naming chosen repeated parts of a
   [val] type with [where].

   - [mlsem/extractGroups] lists, for each [val] line of the definition at
     a position, its type (as [Type_refactor] prints it) and the parts that
     repeat in it, with where each occurrence is in that text.
   - [mlsem/extractApply] extracts the chosen parts of one line, by key and
     name, and returns the new type and the edit that writes it.

   Both work on the parse of the request's text, not on the typecheck. *)

open Mlsem_app
open Yojson.Safe.Util

(* The types of the [val] lines of the target, in order, with their range in
   the document. *)
let sig_types text offsets (t : Signature_check.target) =
  match Main.parse (`String text) with
  | PFailure _ -> []
  | PSuccess program ->
      List.filter_map
        (fun (_, elem) ->
           match elem with
           | PAst.SigDef (_, _, Mlsem_types.TyExpr.TLoc ((s, e), ty))
             when List.exists
                    (fun (vs, ve) -> vs <= s.Lexing.pos_cnum && e.Lexing.pos_cnum <= ve)
                    t.vals ->
               let range =
                 Lsp.Types.Range.create
                   ~start:(Doc.position_of_offset offsets s.pos_cnum)
                   ~end_:(Doc.position_of_offset offsets e.pos_cnum)
               in
               Some (range, ty)
           | _ -> None )
        program

(* A part's identity between the two requests. *)
let key g = Type_refactor.print g

(* mlsem/extractGroups *)
let groups params : Yojson.Safe.t =
  Signature_check.with_target params (fun text offsets t ->
    let lines =
      sig_types text offsets t
      |> List.filter_map (fun (_, ty) ->
        match Type_refactor.checked_print ty with
        | None -> None
        | Some _ ->
            let ty = Type_refactor.strip ty in
            let layout = Type_refactor.layout ty in
            let used = ref (Type_refactor.names ty) in
            let group g =
              let name = Type_refactor.default_name !used g in
              used := name :: !used ;
              let occurrences =
                layout.spans
                |> List.filter (fun (s, _, _) -> s = g)
                |> List.map (fun (_, a, b) -> `List [`Int a; `Int b])
                |> List.sort_uniq compare
              in
              `Assoc
                [
                  ("key", `String (key g)); ("name", `String name);
                  ("holes", `Int (Type_refactor.holes_in g)); ("occurrences", `List occurrences);
                ]
            in
            Some
              (`Assoc
                  [
                    ("text", `String layout.s);
                    ("groups", `List (List.map group (Type_refactor.groups ty)));
                  ] ) )
    in
    `Assoc [("ok", `Bool true); ("name", `String t.name); ("lines", `List lines)] )

(* mlsem/extractApply *)
let apply params : Yojson.Safe.t =
  Signature_check.with_target params (fun text offsets t ->
    let line = params |> member "line" |> to_int in
    let plumbing = params |> member "plumbing" |> to_bool in
    let chosen =
      params |> member "groups" |> to_list
      |> List.map (fun g -> (g |> member "key" |> to_string, g |> member "name" |> to_string))
    in
    let printable =
      sig_types text offsets t
      |> List.filter (fun (_, ty) -> Option.is_some (Type_refactor.checked_print ty))
    in
    match List.nth_opt printable line with
    | None -> Signature_check.error "The signature changed; reopen the toolkit."
    | Some (range, ty) -> (
        let groups = Type_refactor.groups ty in
        let found =
          List.map (fun (k, n) -> (List.find_opt (fun g -> key g = k) groups, n)) chosen
        in
        if List.exists (fun (g, _) -> Option.is_none g) found then
          Signature_check.error "The signature changed; reopen the toolkit."
        else
          let chosen = List.map (fun (g, n) -> (Option.get g, n)) found in
          match Type_refactor.extract_chosen ~plumbing ty chosen with
          | Error msg -> Signature_check.error msg
          | Ok s ->
              let links =
                (not plumbing) && List.exists (fun (g, _) -> Type_refactor.holes_in g > 0) chosen
              in
              `Assoc
                [
                  ("ok", `Bool true); ("text", `String s); ("linksHoles", `Bool links);
                  ( "edit",
                    `Assoc [("range", Lsp.Types.Range.yojson_of_t range); ("newText", `String s)] );
                ] ) )
