(* The toolkit's "Draft holes": the values the holes of a definition's draft
   [val] lines were resolved to, for the editor to fill them. *)

open Yojson.Safe.Util

(* mlsem/holes: the resolved holes of the [val] lines of the definition at
   [position], from the document's typecheck, in order, with the line of
   each. *)
let holes params : Yojson.Safe.t =
  Signature_check.with_target params (fun text offsets t ->
    let uri =
      (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri
    in
    Store.sync_text uri text ;
    let _, _, (typings : Typings.t) = Store.results uri in
    let in_vals (s, e) = List.exists (fun (vs, ve) -> vs <= s && e <= ve) t.vals in
    let range (s, e) =
      Lsp.Types.Range.create
        ~start:(Doc.position_of_offset offsets s)
        ~end_:(Doc.position_of_offset offsets e)
    in
    (* A hole's values: one, or one per overload of its line's result when
       they differ ([value] null where the line cannot be that overload). *)
    let rows (body : Typings.hover_body) =
      match body with
      | Hole {hole; value} -> Some (hole, [`Assoc [("overload", `Null); ("value", `String value)]])
      | Hole_per_overload {hole; rows} ->
          Some
            ( hole,
              List.map
                (fun (o, v) ->
                   `Assoc
                     [
                       ("overload", `String o);
                       ("value", Option.fold ~none:`Null ~some:(fun v -> `String v) v);
                     ] )
                rows )
      | _ -> None
    in
    let holes =
      typings.hovers
      |> List.filter_map (fun (h : Typings.hover) ->
        match rows h.body with
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
