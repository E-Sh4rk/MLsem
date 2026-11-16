(* The toolkit's "Widen": replacing parts of [val] lines' types with wider
   types.

   For each [val] line of the definition at a position, every part of its
   type (as the parser located it), with its wider types and a hole. A wider type is written in place of the part,
   parentheses included, in parentheses when the printer puts it in them
   there.

   The wider types (the part is a subtype of each, and none is a subtype of
   it), tightest first:
   - the part with every literal widened to its base type ([42] and
     intervals to [int], ["a"] to [string], [true] to [bool], ['c'] to
     [char]);
   - each built-in and declared type, a declared type with parameters
     applied to the tightest arguments that make it contain the part
     ([tree(int)] for a tree of [int]s), found by tallying;
   - [any];
   then the hole [_], which Draft holes fills from the definition's body
   (not wider: it can end up narrower than the part).
   Types are built in the environment of the document's typecheck, which
   has its declared types.

   TODO: transform the type and write it back keeping the user's formatting
   ([Type_refactor] prints it anew), so that a replacement is not text put
   in place of a span. *)

open Mlsem_app
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

(* The declared type [name] with [k] parameters, applied to the tightest
   arguments that make it contain [part_ty]: [part_ty ≤ name('_1, …, '_k)]
   tallied, the part's own variables fixed, and each argument its lower
   bound (the variables tally leaves in it [empty]). The arguments are
   printed and parsed back, to be written. [None] when no arguments fit. *)
let applied envs build part_ty name k =
  let names = List.init k (fun i -> "_arg" ^ string_of_int i) in
  match build (TApp (name, List.map (fun n -> TVar (KInfer, n)) names)) with
  | exception _ -> None
  | c_ty -> (
      let args =
        TVOp.vars c_ty |> MVarSet.proj1 |> TVarSet.elements |> List.filter (TVar.has_kind KInfer)
      in
      let arg n = List.find_opt (fun v -> TVar.name v = n) args in
      let mono = MVarSet.union (TVOp.all_vars KNoInfer) (TVOp.vars part_ty) in
      match TVOp.tally mono [(part_ty, c_ty)] with
      | [] -> None
      | s :: _ -> (
          let lower t =
            let t = Subst.apply s t in
            let left =
              TVOp.vars t |> MVarSet.proj1 |> TVarSet.elements
              |> List.filter (fun v -> TVar.has_kind KInfer v && not (MVarSet.mem1 v mono))
            in
            Subst.apply (Subst.of_list1 (List.map (fun v -> (v, Ty.empty)) left)) t
          in
          let print t = Main.display_gty envs (GTy.mk t) in
          match
            List.map
              (fun n ->
                 match arg n with
                 | Some v -> IO.parse_type_string (print (lower (TVar.typ v)))
                 | None -> TBase TAny )
              names
          with
          | exprs -> Some (TApp (name, exprs))
          | exception _ -> None ) )

(* The wider types of the located part [p] of the [val] type [line], as
   written in its place, and the hole; [None] when it cannot be built (an
   optional field [t?]) or is a hole itself. Types are built under the
   line's [where] definitions, so that a part may use their names
   ([Node(x1)] in [... where x1 = ...]). *)
let wider envs line (p : Type_refactor.part) =
  let defs =
    match Type_refactor.strip line with
    | TWhere (_, defs) -> defs
    | _ -> []
  in
  let build t =
    let t = if defs = [] then t else TWhere (t, defs) in
    Typings.build_signature envs t |> fst |> Signature.to_gty |> GTy.ub
  in
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
          let declared n =
            match Main.user_alias_arity envs n with
            | Some k when k > 0 -> (
                match applied envs build part_ty n k with
                | None -> []
                | Some c ->
                    let c = Type_refactor.strip c in
                    let w = widen_literals c in
                    if w <> c then [c; w] else [c] )
            | _ -> [TCustom n]
          in
          let literal = widen_literals node in
          (* Declared types first: of two equivalent candidates the first is
             kept, and a declared name reads better than its structure. *)
          let exprs =
            List.concat_map declared (Main.user_type_names envs)
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
          let hole =
            match node with
            | TVar (KInfer, _) -> []
            | _ -> [("_", Some "hole")]
          in
          match List.map (fun (c, _) -> (written c, None)) sorted @ any @ hole with
          | [] -> None
          | os -> Some os ) )

(* mlsem/widen: the [val] lines of the definition at [position] with their
   widenable parts and the wider types of each ([Type_parts.lines]). *)
let parts params : Yojson.Safe.t = Type_parts.lines params wider
