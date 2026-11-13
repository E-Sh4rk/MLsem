(* The toolkit's "Widen": replacing parts of [val] lines' types with wider
   types.

   For each [val] line of the definition at a position, every part of its
   type (as the parser located it) that has a wider type than [any], with
   its wider types. A wider type is written in place of the part,
   parentheses included, in parentheses when the printer puts it in them
   there.

   The wider types (the part is a subtype of each, and none is a subtype of
   it), tightest first:
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

(* The wider types of the located part [p], as written in its place;
   [None] when it cannot be built on its own (an optional field [t?]) or
   only [any] is wider. *)
let wider envs (p : Type_refactor.part) =
  let benv, _, _, _, _ = envs in
  let build t = Signature.build benv t |> fst |> Signature.to_gty |> GTy.ub in
  match p.node with
  | TOption _ -> None
  | _ -> (
      match build p.node with
      | exception _ -> None
      | part_ty -> (
          let node = Type_refactor.strip p.node in
          let named =
            List.map (fun b -> TBase b) builtin
            @ List.map (fun n -> TCustom n) (Main.user_type_names envs)
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
                        && not (List.exists (fun (_, t) -> Ty.leq t c_ty && Ty.leq c_ty t) acc) ->
                     acc @ [(c, c_ty)]
                 | _ -> acc
                 | exception _ -> acc )
              [] exprs
          in
          (* Tightest first: by how many of the others are wider. *)
          let wider_than (_, t) = List.length (List.filter (fun (_, t') -> Ty.leq t t') wider) in
          let sorted = List.stable_sort (fun a b -> compare (wider_than b) (wider_than a)) wider in
          let written c =
            match p.parent with
            | Some (parent, i) -> Type_refactor.print_as_child parent i c
            | None -> Type_refactor.print c
          in
          match sorted with
          | [] -> None
          | _ ->
              let any = if Ty.leq Ty.any part_ty then [] else [written (TBase TAny)] in
              Some (List.map (fun (c, _) -> written c) sorted @ any) ) )

(* mlsem/widen: the [val] lines of the definition at [position] with their
   widenable parts: per line, its number and text, and per part its columns
   in the line, its text and its wider types. *)
let parts params : Yojson.Safe.t =
  Signature_check.with_target params (fun text offsets (t : Signature_check.target) ->
    let uri =
      (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri
    in
    Store.sync_text uri text ;
    match (Store.envs_of uri, Main.parse (`String text)) with
    | None, _ -> error "The document's typecheck failed: its types are not known."
    | _, PFailure _ -> error "The file does not parse."
    | Some envs, PSuccess program ->
        let in_vals s = List.exists (fun (vs, ve) -> vs <= s && s < ve) t.vals in
        let lines =
          List.filter_map
            (fun (_, elem) ->
               match elem with
               | PAst.SigDef (_, _, (TLoc ((s, _), _) as ty)) when in_vals s.Lexing.pos_cnum ->
                   let line = (Doc.position_of_offset offsets s.pos_cnum).line in
                   let line_start = offsets.(line) in
                   let line_end =
                     match String.index_from_opt text line_start '\n' with
                     | Some i -> i
                     | None -> String.length text
                   in
                   let parts =
                     Type_refactor.located_parts ty
                     |> List.filter_map (fun (p : Type_refactor.part) ->
                       let s, e = p.span in
                       (* A part on this line only. *)
                       if e > line_end then
                         None
                       else
                         Option.map
                           (fun ws ->
                              `Assoc
                                [
                                  ("start", `Int (s - line_start)); ("end", `Int (e - line_start));
                                  ("text", `String (String.sub text s (e - s)));
                                  ("wider", `List (List.map (fun w -> `String w) ws));
                                ] )
                           (wider envs p) )
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
