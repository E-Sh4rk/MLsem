(* What the toolkit's part tools (Widen, Simplify) share: the [val] lines of
   the definition at a position, and per line the parts of its type that a
   tool has options for. A part is located by the parser; an option is
   written in its place, parentheses included, in parentheses when the
   printer puts it in them there. *)

open Mlsem_app
open Yojson.Safe.Util

let error msg = `Assoc [("ok", `Bool false); ("error", `String msg)]

(* [c] as written in place of the part [p]. *)
let written (p : Type_refactor.part) c =
  match p.parent with
  | Some (parent, i) -> Type_refactor.print_as_child parent i c
  | None -> Type_refactor.print c

(* The [val] lines of the definition at [position]: per line, its number and
   text, and per part (on that line) its columns, its text and its options,
   each as written and with a label if any. [options envs line part] gives
   them, [None] for none: [line] is the line's whole type. *)
let lines params options : Yojson.Safe.t =
  Signature_check.with_target params (fun text offsets (t : Signature_check.target) ->
    let uri =
      (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri
    in
    (* Only the types matter: the declarations' environment, without
       typechecking the definitions. *)
    match (Store.declarations uri text, Main.parse (`String text)) with
    | None, _
    | _, PFailure _ ->
        error "The file does not parse."
    | Some (envs, _), PSuccess program ->
        let in_vals s = List.exists (fun (vs, ve) -> vs <= s && s < ve) t.vals in
        let lines =
          List.filter_map
            (fun (_, elem) ->
               match elem with
               | PAst.SigDef (_, _, (Mlsem_types.TyExpr.TLoc ((s, _), _) as ty))
                 when in_vals s.Lexing.pos_cnum ->
                   let line = (Doc.position_of_offset offsets s.pos_cnum).line in
                   let line_start = offsets.(line) in
                   let line_end =
                     match String.index_from_opt text line_start '\n' with
                     | Some i -> i
                     | None -> String.length text
                   in
                   let option (text, label) =
                     `Assoc
                       ( ("text", `String text)
                       :: Option.fold ~none:[] ~some:(fun l -> [("label", `String l)]) label )
                   in
                   let parts =
                     Type_refactor.located_parts ty
                     |> List.filter_map (fun (p : Type_refactor.part) ->
                       let s, e = p.span in
                       (* A part on this line only. *)
                       if e > line_end then
                         None
                       else
                         match
                           options envs ty p
                         with
                         | None
                         | Some [] ->
                             None
                         | Some os ->
                             Some
                               (`Assoc
                                   [
                                     ("start", `Int (s - line_start));
                                     ("end", `Int (e - line_start));
                                     ("text", `String (String.sub text s (e - s)));
                                     ("options", `List (List.map option os));
                                   ] ) )
                   in
                   Some
                     (`Assoc
                         [
                           ("line", `Int line);
                           ("text", `String (String.sub text line_start (line_end - line_start)));
                           ("parts", `List parts);
                         ] )
               | _ -> None )
            program
        in
        `Assoc [("ok", `Bool true); ("lines", `List lines)] )
