(* The toolkit's "Widen": replacing parts of [val] lines' types with wider
   types.

   For each [val] line of the definition at a position, every part of its
   type (as the parser located it), with its wider types and a wildcard. A
   wider type is written in place of the part, parentheses included, in
   parentheses when the printer puts it in them there.

   The wider types (the part is a subtype of each, and none is a subtype of
   it), tightest first:
   - the part with every literal widened to its base type ([42] and
     intervals to [int], ["a"] to [string], [true] to [bool], ['c'] to
     [char]);
   - each built-in and declared type, a declared type with parameters
     applied to the tightest arguments that make it contain the part
     ([tree(int)] for a tree of [int]s), found by tallying;
   - [any];
   then the wildcard [_], which Partial signatures fills from the
   definition's body (not wider: it can end up narrower than the part).
   Types are built in the environment of the document's typecheck, which
   has its declared types.

   TODO: transform the type and write it back keeping the user's formatting
   ([Type_refactor] prints it anew), so that a replacement is not text put
   in place of a span. *)

open Mlsem_types
open TyExpr

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

(* The wider types of the located part [p] of the [val] type [line], as
   written in its place, and the wildcard; [None] when it cannot be built (an
   optional field [t?]) or is a wildcard itself. *)
let wider envs line (p : Type_refactor.part) =
  let build = Type_parts.build envs line in
  match p.node with
  | TOption _ -> None
  | _ -> (
      match build p.node with
      | exception _ -> None
      | part_ty -> (
          let node = Type_refactor.strip p.node in
          (* A declared type with parameters is offered with the arguments
             that fit the part, and with those arguments' literals widened
             ([box(bool)] for [Box(true)], where [box(true)] is the part
             itself). *)
          let declared =
            Type_parts.declared envs build part_ty
            |> List.concat_map (fun c ->
              let w = widen_literals c in
              if w <> c then [c; w] else [c] )
          in
          let literal = widen_literals node in
          (* Declared types first: of two equivalent candidates the first is
             kept, and a declared name reads better than its structure. *)
          let exprs =
            declared
            @ (if literal <> node then [literal] else [])
            @ List.map (fun b -> TBase b) builtin
          in
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
          let written = Type_parts.written p in
          let any = if Ty.leq Ty.any part_ty then [] else [(written (TBase TAny), None)] in
          let wildcard =
            match node with
            | TVar (KInfer, _) -> []
            | _ -> [("_", Some "wildcard")]
          in
          match List.map (fun (c, _) -> (written c, None)) sorted @ any @ wildcard with
          | [] -> None
          | os -> Some os ) )

(* mlsem/widen: the [val] lines of the definition at [position] with their
   widenable parts and the wider types of each ([Type_parts.lines]). *)
let parts params : Yojson.Safe.t = Type_parts.lines params wider
