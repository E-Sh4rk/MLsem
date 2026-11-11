(* Checking a definition against candidate [val] declarations, for the
   editor's "Infer type with LLM" command.

   The editor asks an LLM for [val] lines, sends them here, and feeds the
   errors back to the LLM until the definition checks. Everything here works
   from the parse tree, not from a typecheck of the document: the definition
   may not typecheck at all yet (that is often why the user asks), or take too
   long to infer without a [val].

   - [mlsem/definitionAt] finds the top-level definition at a position and
     returns its name, source and first line, for the prompt.
   - [mlsem/checkSignature] writes the candidate [val] lines above the
     definition, in place of the existing ones, and typechecks the source up
     to the definition followed by the later [val] and type declarations. A
     definition may use a later definition that has a [val], so those are
     needed, but the later definitions themselves cannot change its typing.
     It returns the errors in the [val] lines and the definition, or on
     success the edit that writes the [val] lines into the document. *)

open Mlsem_common
open Mlsem_app
open Yojson.Safe.Util

type target = {
  name: string;
  line_start: int;  (** start of the definition's first line *)
  stop: int;  (** end of the definition *)
  vals: (int * int) list;  (** the existing [val] lines of [name], as whole lines, in order *)
  later_decls: string;  (** the [val] and type declarations after the definition, as whole lines *)
}

let span pos =
  if pos = Position.dummy then
    None
  else
    Some
      ( Position.offset (Position.start_of_position pos),
        Position.offset (Position.end_of_position pos) )

let line_start text off =
  match String.rindex_from_opt text (off - 1) '\n' with
  | Some i -> i + 1
  | None -> 0
  | exception Invalid_argument _ -> 0

let line_end text off =
  match String.index_from_opt text off '\n' with
  | Some i -> i + 1
  | None -> String.length text

(* The top-level definition containing [off]. In a [let ... and ...] group,
   the member whose binding contains [off], or the first one. The [val] lines
   of its name are those written since the last definition of that name. *)
let find_target text (program : PAst.program) off =
  let binds name = function
    | PAst.Definitions lst ->
        List.exists (fun (_, (_, lname), _) -> Position.value lname = name) lst
    | _ -> false
  in
  let rec go before = function
    | [] -> None
    | ((annot, elem) as el) :: rest -> (
        match (elem, span (Position.position annot)) with
        | PAst.Definitions ((_, (_, first), _) :: _ as lst), Some (s, e) when s <= off && off <= e
          ->
            let name =
              lst
              |> List.find_map (fun (bpos, (_, lname), _) ->
                match span bpos with
                | Some (s, e) when s <= off && off <= e -> Some (Position.value lname)
                | _ -> None )
              |> Option.value ~default:(Position.value first)
            in
            let vals =
              List.rev before
              |> List.fold_left
                   (fun acc (annot, elem) ->
                      match elem with
                      | PAst.SigDef (n, _, _) when n = name -> (
                          match span (Position.position annot) with
                          | Some (s, e) -> (line_start text s, line_end text e) :: acc
                          | None -> acc )
                      | elem when binds name elem -> []
                      | _ -> acc )
                   []
            in
            (* A definition can use a later one that has a [val], even one
               written after it, so later declarations are kept. *)
            let later_decls =
              rest
              |> List.filter_map (fun (annot, elem) ->
                match (elem, span (Position.position annot)) with
                | (PAst.SigDef _ | Types _ | AbsType _), Some (s, e) ->
                    let s = line_start text s in
                    Some (String.sub text s (line_end text e - s))
                | _ -> None )
              |> String.concat ""
            in
            Some {name; line_start = line_start text s; stop = e; vals = List.rev vals; later_decls}
        | _ -> go (el :: before) rest )
  in
  go [] program

let without_spans text spans ~from ~until =
  let b = Buffer.create (until - from) in
  let pos =
    List.fold_left
      (fun pos (s, e) ->
         if s >= pos && e <= until then begin
           Buffer.add_string b (String.sub text pos (s - pos)) ;
           e
         end
         else
           pos )
      from spans
  in
  Buffer.add_string b (String.sub text pos (until - pos)) ;
  Buffer.contents b

let decode params =
  let text = params |> member "text" |> to_string in
  let pos = params |> member "position" |> Lsp.Types.Position.t_of_yojson in
  let offsets = Doc.compute_line_offsets text in
  (text, offsets, Doc.offset_of_position offsets (String.length text) pos)

let error msg = `Assoc [("ok", `Bool false); ("error", `String msg)]

let with_target params k =
  try
    let text, offsets, off = decode params in
    match Main.parse (`String text) with
    | PFailure (_, msg) -> error ("The file does not parse: " ^ msg)
    | PSuccess program -> (
        match find_target text program off with
        | None -> error "No top-level definition here."
        | Some t -> k text offsets t )
  with
  | e -> error (Printexc.to_string e)

(* mlsem/definitionAt *)
let definition_at params : Yojson.Safe.t =
  with_target params (fun text _ t ->
    `Assoc
      [
        ("ok", `Bool true); ("name", `String t.name);
        ("definition", `String (String.sub text t.line_start (t.stop - t.line_start)));
        ("line", `Int (Doc.position_of_offset (Doc.compute_line_offsets text) t.line_start).line);
      ] )

(* The text of [pos] in [text], shortened, to show the LLM where an error is. *)
let excerpt text (r : Lsp.Types.Range.t) =
  let offsets = Doc.compute_line_offsets text in
  let len = String.length text in
  let s = Doc.offset_of_position offsets len r.start in
  let e = Doc.offset_of_position offsets len r.end_ in
  let ex = String.sub text s (max 0 (e - s)) |> String.split_on_char '\n' |> String.concat " " in
  if String.length ex > 80 then String.sub ex 0 77 ^ "..." else ex

(* mlsem/checkSignature *)
let check_signature params : Yojson.Safe.t =
  with_target params (fun text offsets t ->
    let indent = Doc.indent_at text offsets t.line_start in
    let decls =
      params |> member "types" |> to_list |> List.map to_string
      |> List.map (fun ty -> indent ^ "val " ^ t.name ^ " : " ^ String.trim ty ^ "\n")
      |> String.concat ""
    in
    (* The first line the edit touches: the first [val] line, or the
       definition. *)
    let from =
      match t.vals with
      | (s, _) :: _ -> s
      | [] -> t.line_start
    in
    let kept = without_spans text t.vals ~from ~until:t.line_start in
    let prefix = String.sub text 0 from ^ kept in
    let lines s = List.length (String.split_on_char '\n' s) - 1 in
    let checked = prefix ^ decls ^ String.sub text t.line_start (t.stop - t.line_start) ^ "\n" in
    let candidate = checked ^ t.later_decls in
    (* Errors from the [val] lines to the end of the definition. *)
    let first_line = lines prefix and last_line = lines checked in
    let result = Typecheck.run candidate in
    let errors =
      result.diagnostics
      |> List.filter (fun (d : Lsp.Types.Diagnostic.t) ->
        d.severity = Some Lsp.Types.DiagnosticSeverity.Error
        && d.range.start.line >= first_line && d.range.start.line < last_line )
      |> List.rev_map (fun (d : Lsp.Types.Diagnostic.t) ->
        let msg =
          match d.message with
          | `String s -> s
          | `MarkupContent m -> m.value
        in
        match excerpt candidate d.range with
        | "" -> `String msg
        | ex -> `String (Printf.sprintf "at `%s`: %s" ex msg) )
    in
    let declared =
      List.exists
        (fun (b : Typecheck.binding) ->
           b.name = t.name && b.declared && b.def_start >= String.length prefix )
        result.bindings
    in
    if errors = [] && declared then
      let range =
        Lsp.Types.Range.create
          ~start:(Doc.position_of_offset offsets from)
          ~end_:(Doc.position_of_offset offsets t.line_start)
      in
      `Assoc
        [
          ("ok", `Bool true);
          ( "edit",
            `Assoc
              [("range", Lsp.Types.Range.yojson_of_t range); ("newText", `String (kept ^ decls))] );
        ]
    else
      let errors =
        if errors = [] then
          [`String "The definition does not check against these types."]
        else
          errors
      in
      `Assoc [("ok", `Bool false); ("errors", `List errors)] )
