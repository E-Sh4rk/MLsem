(* Generalize several overloads into one polymorphic type by anti-unification
   (Plotkin, Reynolds), on the type expressions the overloads print as.

   The overloads are walked in parallel. Where their constructors agree
   (arrow, tuple, applied type, tag, list, connective), the constructor is kept
   and the walk goes on in the children. Where they disagree, a type variable
   takes their place, and every position that disagrees with the same tuple of
   instances gets the same variable: that is how [int -> int] and
   [float -> float] become ['a -> 'a].

   A variable is bounded by the union of its instances, written as an
   intersection: [('a & (int | float)) -> ('a & (int | float))]. A variable
   that occurs once correlates nothing, so its bound is written alone.

   This works on syntax, not on the semantics of the types: equivalent types
   printed differently count as different, and all the overloads are
   generalized together (no choosing of subsets). The result claims more than
   the overloads did, so it has to be checked against the definition. *)

open Mlsem_types
open TyExpr

let rec strip = function
  | TLoc (_, t) -> strip t
  | (TDyn | TVar _ | TRowVar _ | TBase _ | TEnum _ | TCustom _) as t -> t
  | TApp (n, ts) -> TApp (n, List.map strip ts)
  | TTag (n, t) -> TTag (n, strip t)
  | TTuple ts -> TTuple (List.map strip ts)
  | TRecord (fields, t) -> TRecord (List.map (fun (l, t) -> (l, strip t)) fields, strip t)
  | TSList r -> TSList (strip_re r)
  | TCons (a, b) -> TCons (strip a, strip b)
  | TArrow (a, b) -> TArrow (strip a, strip b)
  | TOption t -> TOption (strip t)
  | TCup (a, b) -> TCup (strip a, strip b)
  | TCap (a, b) -> TCap (strip a, strip b)
  | TDiff (a, b) -> TDiff (strip a, strip b)
  | TNeg t -> TNeg (strip t)
  | TWhere (t, defs) -> TWhere (strip t, List.map (fun (n, ps, t) -> (n, ps, strip t)) defs)
  | TRecUpd (t, fields) -> TRecUpd (strip t, List.map (fun (l, t) -> (l, strip t)) fields)
  | TRecProj (t, l) -> TRecProj (strip t, l)
  | TTagProj (t, l) -> TTagProj (strip t, l)

and strip_re = function
  | Epsilon -> Epsilon
  | Symbol t -> Symbol (strip t)
  | Union rs -> Union (List.map strip_re rs)
  | Concat rs -> Concat (List.map strip_re rs)
  | Star r -> Star (strip_re r)
  | Plus r -> Plus (strip_re r)
  | Option r -> Option (strip_re r)

(* A regular expression's symbols in order, and its skeleton: the expression
   with every symbol replaced by the same placeholder. Two lists with the
   same skeleton are walked symbol by symbol. *)
let rec symbols = function
  | Epsilon -> []
  | Symbol t -> [t]
  | Union rs
  | Concat rs ->
      List.concat_map symbols rs
  | Star r
  | Plus r
  | Option r ->
      symbols r

let rec skeleton = function
  | Epsilon -> Epsilon
  | Symbol _ -> Symbol (TBase TAny)
  | Union rs -> Union (List.map skeleton rs)
  | Concat rs -> Concat (List.map skeleton rs)
  | Star r -> Star (skeleton r)
  | Plus r -> Plus (skeleton r)
  | Option r -> Option (skeleton r)

(* Put [ts] back in place of the symbols of [r], in order. *)
let fill r ts =
  let rest = ref ts in
  let rec go = function
    | Epsilon -> Epsilon
    | Symbol _ -> (
        match !rest with
        | t :: tl ->
            rest := tl ;
            Symbol t
        | [] -> assert false )
    | Union rs -> Union (List.map go rs)
    | Concat rs -> Concat (List.map go rs)
    | Star r -> Star (go r)
    | Plus r -> Plus (go r)
    | Option r -> Option (go r)
  in
  go r

type shape =
  | Arrow
  | Tuple of int
  | App of string * int
  | Tag of string
  | Cons
  | Opt
  | SList of regexp
  | Cup
  | Cap
  | Diff
  | Neg

(* A type's outer constructor and its children, for the constructors the
   walk goes through. *)
let split = function
  | TArrow (a, b) -> Some (Arrow, [a; b])
  | TTuple ts -> Some (Tuple (List.length ts), ts)
  | TApp (n, ts) -> Some (App (n, List.length ts), ts)
  | TTag (n, t) -> Some (Tag n, [t])
  | TCons (a, b) -> Some (Cons, [a; b])
  | TOption t -> Some (Opt, [t])
  | TSList r -> Some (SList (skeleton r), symbols r)
  | TCup (a, b) -> Some (Cup, [a; b])
  | TCap (a, b) -> Some (Cap, [a; b])
  | TDiff (a, b) -> Some (Diff, [a; b])
  | TNeg t -> Some (Neg, [t])
  | _ -> None

let join shape children =
  match (shape, children) with
  | Arrow, [a; b] -> TArrow (a, b)
  | Tuple _, ts -> TTuple ts
  | App (n, _), ts -> TApp (n, ts)
  | Tag n, [t] -> TTag (n, t)
  | Cons, [a; b] -> TCons (a, b)
  | Opt, [t] -> TOption t
  | SList r, ts -> TSList (fill r ts)
  | Cup, [a; b] -> TCup (a, b)
  | Cap, [a; b] -> TCap (a, b)
  | Diff, [a; b] -> TDiff (a, b)
  | Neg, [t] -> TNeg t
  | _ -> assert false

(* The columns of equally long rows. *)
let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | rows -> List.map List.hd rows :: transpose (List.map List.tl rows)

let rec type_vars acc = function
  | TVar (_, n) -> n :: acc
  | t -> (
      match split t with
      | Some (_, children) -> List.fold_left type_vars acc children
      | None -> acc )

let var_name i =
  let letter = String.make 1 (Char.chr (Char.code 'a' + (i mod 26))) in
  if i < 26 then letter else letter ^ string_of_int (i / 26)

let union = function
  | [] -> TBase TEmpty
  | t :: ts -> List.fold_left (fun acc t -> TCup (acc, t)) t ts

type variable = {
  name: string;
  instances: TyExpr.t list;
  occurrences: int;
}

type result = {
  ty: TyExpr.t;
  vars: variable list;  (** in order of first occurrence *)
}

let generalize (overloads : TyExpr.t list) : result =
  let ts = List.map strip overloads in
  let used = List.fold_left type_vars [] ts in
  (* Each variable, keyed by its tuple of instances (one per overload). *)
  let vars : (TyExpr.t list * (string * int ref)) list ref = ref [] in
  let next = ref 0 in
  let rec fresh () =
    let n = var_name !next in
    incr next ;
    if List.mem n used then fresh () else n
  in
  let var instances =
    let name =
      match List.assoc_opt instances !vars with
      | Some (name, count) -> incr count ; name
      | None ->
          let name = fresh () in
          vars := !vars @ [(instances, (name, ref 1))] ;
          name
    in
    TVar (KNoInfer, name)
  in
  let rec au = function
    | [] -> TBase TEmpty
    | t :: rest when List.for_all (( = ) t) rest -> t
    | ts -> (
        match List.map split ts with
        | Some (shape, _) :: _ as splits
          when List.for_all
                 (function
                   | Some (s, _) -> s = shape
                   | None -> false )
                 splits ->
            let children = List.filter_map (Option.map snd) splits in
            join shape (List.map au (transpose children))
        | _ -> var ts )
  in
  let ty = au ts in
  let vars =
    List.map
      (fun (instances, (name, count)) ->
         {name; instances = List.sort_uniq compare instances; occurrences = !count} )
      !vars
  in
  (* Bound every generated variable by its instances. *)
  let rec bound t =
    match t with
    | TVar (_, n) -> (
        match List.find_opt (fun v -> v.name = n) vars with
        | Some v when v.occurrences > 1 -> TCap (t, union v.instances)
        | Some v -> union v.instances
        | None -> t )
    | _ -> (
        match split t with
        | Some (shape, children) -> join shape (List.map bound children)
        | None -> t )
  in
  {ty = bound ty; vars}

(* How a generalized type compares with the collapse of the same overloads. *)
type precision =
  | Equivalent
  | More_precise  (** some instance of it is below the collapse, not conversely *)
  | Less_precise
  | Incomparable

(* Whether [t] can stand for [s]: an intersection of [copies] instances of [t]
   is a subtype of [s], with the variables of [s] held rigid. This is
   expansion followed by subsumption: a polymorphic type is used at several
   instances at once, which a single instance cannot do. For example
   ['a & B -> ('a & B, 'a & B)] is below [(int, int) | (bool, bool)] only as
   the intersection of its [int] and [bool] instances. Each copy gets fresh
   variables, and tallying solves for all of them together. *)
let derives ~copies (t : Ty.t) (s : Ty.t) =
  let vars = TVOp.vars t in
  let instances =
    if MVarSet.is_empty vars then
      t
    else
      List.init copies (fun _ -> Subst.apply (TVOp.refresh ~kind:KInfer vars) t) |> Ty.conj
  in
  TVOp.tally ~record:false (TVOp.vars s) [(instances, s)] <> []

(* [copies] instances are tried for each side: as many as there are
   overloads, which is enough whenever every overload is an instance of the
   generalized type, as anti-unification makes it. A type that needs more
   instances than that to stand for the other is reported as not standing for
   it. *)
let compare_with_collapse ~copies ~generalized ~collapsed =
  match (derives ~copies generalized collapsed, derives ~copies collapsed generalized) with
  | true, true -> Equivalent
  | true, false -> More_precise
  | false, true -> Less_precise
  | false, false -> Incomparable

let precision_name = function
  | Equivalent -> "equivalent"
  | More_precise -> "more"
  | Less_precise -> "less"
  | Incomparable -> "incomparable"
