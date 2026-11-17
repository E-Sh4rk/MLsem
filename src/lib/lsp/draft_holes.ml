(* The toolkit's "Draft holes": the values the holes of a definition's draft
   [val] lines were resolved to, for the editor to fill them. *)

open Yojson.Safe.Util

(* The parts of a hole's [value] that may be kept as holes, located as the
   parts of a [val] line are ([Type_parts.locate]): not the whole value
   (keeping it is not filling the hole), an optional field's [t?] (a hole
   cannot be optional), nor a hole. Keeping any of them is safe: the whole
   value is one solution of the holes left. *)
let kept_parts value =
  match Type_parts.locate value with
  | exception _ -> []
  | _, parts ->
      List.tl parts
      |> List.filter (fun (p : Type_refactor.part) ->
        match p.node with
        | Mlsem_types.TyExpr.TOption _ -> false
        | t -> not (Type_refactor.is_hole (Type_refactor.strip t)) )
      |> List.sort_uniq (fun (p : Type_refactor.part) (q : Type_refactor.part) ->
        compare p.span q.span )
      |> List.map (fun p -> Type_parts.part_json value p [])

let parse s =
  match Type_refactor.strip (Mlsem_app.IO.parse_type_string s) with
  | t -> Some t
  | exception _ -> None

(* [value] as written in place of the hole at [hs, he) of the [val] type
   [ty_text]: in parentheses only when the type does not parse to the same
   without them. The printed value may come in parentheses it does not need
   there ([(Node(a, b))] after [->]), or without ones it needs. *)
let as_written ty_text (hs, he) value =
  let n = String.length value in
  let bare =
    if n >= 2 && value.[0] = '(' && value.[n - 1] = ')' then
      let inner = String.sub value 1 (n - 2) in
      match parse inner with
      | Some t when parse value = Some t -> inner
      | _ -> value
    else
      value
  in
  let put v = String.sub ty_text 0 hs ^ v ^ String.sub ty_text he (String.length ty_text - he) in
  let wrapped = "(" ^ bare ^ ")" in
  match parse (put wrapped) with
  | Some t when parse (put bare) = Some t -> bare
  | _ -> wrapped

(* For the document [text]: [value] as written in place of the hole at
   [start, end) (offsets in [text]), by [as_written] in the [val] type
   around it; as it is when no [val] type is around it. *)
let writer text =
  let val_types =
    match Mlsem_app.Main.parse (`String text) with
    | PSuccess program ->
        List.filter_map
          (fun (_, elem) ->
             match elem with
             | Mlsem_app.PAst.SigDef (_, _, Mlsem_types.TyExpr.TLoc ((s, e), _)) ->
                 Some (s.Lexing.pos_cnum, e.Lexing.pos_cnum)
             | _ -> None )
          program
    | PFailure _ -> []
  in
  fun (hs, he) value ->
    match List.find_opt (fun (s, e) -> s <= hs && he <= e) val_types with
    | Some (s, e) -> as_written (String.sub text s (e - s)) (hs - s, he - s) value
    | None -> value

(* mlsem/holes:the resolved holes of the [val] lines of the definition at
   [position], from the document's typecheck, in order, with the line of
   each. A value is as written in place of its hole ([as_written]), and
   comes with its [parts] ([kept_parts]), as [start, end) in it and their
   text. *)
let holes params : Yojson.Safe.t =
  Signature_check.with_target params (fun text offsets t ->
    let uri =
      (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri
    in
    Store.sync_text uri text ;
    let _, _, (typings : Typings.t) = Store.results uri in
    let write = writer text in
    (* [value] as written in place of the hole [h]. *)
    let written (h : Typings.hover) value = write (h.h_start, h.h_end) value in
    let in_vals (s, e) = List.exists (fun (vs, ve) -> vs <= s && e <= ve) t.vals in
    let range (s, e) =
      Lsp.Types.Range.create
        ~start:(Doc.position_of_offset offsets s)
        ~end_:(Doc.position_of_offset offsets e)
    in
    (* A hole's values: one, or one per overload of its line's result when
       they differ ([value] null where the line cannot be that overload). *)
    let row overload value =
      `Assoc
        [
          ("overload", overload);
          ("value", Option.fold ~none:`Null ~some:(fun v -> `String v) value);
          ("parts", `List (Option.fold ~none:[] ~some:kept_parts value));
        ]
    in
    let rows h (body : Typings.hover_body) =
      match body with
      | Hole {hole; value} -> Some (hole, [row `Null (Some (written h value))])
      | Hole_per_overload {hole; rows} ->
          Some (hole, List.map (fun (o, v) -> row (`String o) (Option.map (written h) v)) rows)
      | _ -> None
    in
    let holes =
      typings.hovers
      |> List.filter_map (fun (h : Typings.hover) ->
        match rows h h.body with
        | Some (hole, rows) when in_vals (h.h_start, h.h_end) ->
            let line_s = Signature_check.line_start text h.h_start in
            (* Without its newline, so that the hole's characters index it. *)
            let line_e = Signature_check.line_end text h.h_start in
            let line_e =
              if line_e > line_s && text.[line_e - 1] = '\n' then line_e - 1 else line_e
            in
            let line = String.sub text line_s (line_e - line_s) in
            Some
              ( h.h_start,
                `Assoc
                  [
                    ("range", Lsp.Types.Range.yojson_of_t (range (h.h_start, h.h_end)));
                    ("hole", `String hole); ("rows", `List rows); ("lineText", `String line);
                  ] )
        | _ -> None )
      |> List.sort_uniq (fun (a, _) (b, _) -> compare a b)
      |> List.map snd
    in
    `Assoc [("ok", `Bool true); ("name", `String t.name); ("holes", `List holes)] )
