(* The toolkit's "Widen": replacing a part of a [val] line's type with a
   wider type.

   The part is the smallest part of the type whose location contains the
   selection (or the cursor), as the parser located it. A candidate is
   written in its place, parentheses included, in parentheses when the
   printer puts it in them there.

   The candidates are wider than the part (it is a subtype of each, and
   none is a subtype of it), tightest first:
   - the part with every literal widened to its base type ([42] and
     intervals to [int], ["a"] to [string], [true] to [bool], ['c'] to
     [char]);
   - each built-in and declared type;
   - [any].
   Types are built in the environment of the document's typecheck, which
   has its declared types.

   TODO: transform the type and write it back keeping the user's formatting
   ([Type_refactor] prints it anew), so that a replacement is not text put
   in place of a span. *)

open Mlsem_app
open Mlsem_types
open TyExpr
open Yojson.Safe.Util

let error msg = `Assoc [("ok", `Bool false); ("error", `String msg)]

let builtin = [TInt (None, None); TBool; TChar; TString; TFloat]

(* [t] with every literal widened to its base type. Members of a union that
   become the same are kept once ([true | false] gives [bool]). *)
let widen_literals t =
  let rec go t =
    match t with
    | TBase (TInt _) -> TBase (TInt (None, None))
    | TBase (TCharInt _) -> TBase TChar
    | TBase (TSString _) -> TBase TString
    | TBase (TTrue | TFalse) -> TBase TBool
    | TCup _ ->
        let rec members = function
          | TCup (a, b) -> members a @ members b
          | t -> [go t]
        in
        let ms =
          List.fold_left (fun acc m -> if List.mem m acc then acc else acc @ [m]) [] (members t)
        in
        List.fold_left (fun acc m -> TCup (acc, m)) (List.hd ms) (List.tl ms)
    | t -> Type_refactor.map_children go t
  in
  go t

(* mlsem/widen: for the part of a [val] line's type at [range], its range and
   the wider types, each as written in its place. *)
let candidates params : Yojson.Safe.t =
  try
    let uri =
      (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri
    in
    let text = params |> member "text" |> to_string in
    let range = params |> member "range" |> Lsp.Types.Range.t_of_yojson in
    let offsets = Doc.compute_line_offsets text in
    let off = Doc.offset_of_position offsets (String.length text) in
    let lo, hi =
      let a = off range.start and b = off range.end_ in
      (min a b, max a b)
    in
    let part =
      match Main.parse (`String text) with
      | PFailure _ -> None
      | PSuccess program ->
          List.find_map
            (fun (_, elem) ->
               match elem with
               | PAst.SigDef (_, _, ty) -> Type_refactor.part_at (lo, hi) ty
               | _ -> None )
            program
    in
    match part with
    | None -> error "Not in the type of a val line."
    | Some {node = TOption _; _} -> error "An optional field: select its type."
    | Some {span = s, e; node; parent} -> (
        Store.sync_text uri text ;
        match Store.envs_of uri with
        | None -> error "The document's typecheck failed: its types are not known."
        | Some envs -> (
            let benv, _, _, _, _ = envs in
            let build t = Signature.build benv t |> fst |> Signature.to_gty |> GTy.ub in
            match build node with
            | exception _ -> error "This part cannot be built on its own."
            | part_ty ->
                let node = Type_refactor.strip node in
                let named =
                  List.map (fun b -> TBase b) builtin
                  @ List.map (fun n -> TCustom n) (Main.user_type_names envs)
                  @ [TBase TAny]
                in
                let literal = widen_literals node in
                let exprs = (if literal <> node then [literal] else []) @ named in
                (* Wider than the part, and not equivalent to one kept before. *)
                let wider =
                  List.fold_left
                    (fun acc c ->
                       match build c with
                       | c_ty
                         when Ty.leq part_ty c_ty
                              && (not (Ty.leq c_ty part_ty))
                              && not
                                   (List.exists (fun (_, t) -> Ty.leq t c_ty && Ty.leq c_ty t) acc)
                         ->
                           acc @ [(c, c_ty)]
                       | _ -> acc
                       | exception _ -> acc )
                    [] exprs
                in
                (* Tightest first: by how many of the others are wider. *)
                let wider_than (_, t) =
                  List.length (List.filter (fun (_, t') -> Ty.leq t t') wider)
                in
                let sorted =
                  List.stable_sort (fun a b -> compare (wider_than b) (wider_than a)) wider
                in
                let written c =
                  match parent with
                  | Some (p, i) -> Type_refactor.print_as_child p i c
                  | None -> Type_refactor.print c
                in
                let start = Doc.position_of_offset offsets s in
                let line_start = offsets.(start.line) in
                let line_end =
                  match String.index_from_opt text line_start '\n' with
                  | Some i -> i
                  | None -> String.length text
                in
                `Assoc
                  [
                    ("ok", `Bool true); ("part", `String (String.sub text s (e - s)));
                    ( "range",
                      Lsp.Types.Range.yojson_of_t
                        (Lsp.Types.Range.create ~start ~end_:(Doc.position_of_offset offsets e)) );
                    ("lineText", `String (String.sub text line_start (line_end - line_start)));
                    ("candidates", `List (List.map (fun (c, _) -> `String (written c)) sorted));
                  ] ) )
  with
  | e -> error (Printexc.to_string e)
