open Base
open Tvar

type t = { lb:Ty.t ; ub:Ty.t ; eq:bool }
let mk ty = { lb=ty ; ub=ty ; eq=true }
let mk_gradual lb ub = { lb ; ub ; eq=Ty.leq ub lb }
let empty = mk Ty.empty
let any = mk Ty.any
let dyn = mk_gradual Ty.empty Ty.any
let lb t = t.lb
let ub t = t.ub
let destruct t = t.lb, t.ub
let map_ f flb fub t =
  if t.eq then f t.lb |> mk else mk_gradual (flb t.lb) (fub t.ub)
let map f = map_ f f f
let map2_ f flb fub t1 t2 =
  if t1.eq && t2.eq then
    f t1.lb t2.lb |> mk
  else
    mk_gradual (flb t1.lb t2.lb) (fub t1.ub t2.ub)
let map2 f = map2_ f f f
let mapl_ f flb fub ts =
  if List.for_all (fun t -> t.eq) ts then
    ts |> List.map (fun t -> t.lb) |> f |> mk
  else
    mk_gradual
      (ts |> List.map (fun t -> t.lb) |> flb)
      (ts |> List.map (fun t -> t.ub) |> fub)
let mapl f = mapl_ f f f
let op check f t =
  let f' t = if check t then f t else raise Exit in
  try Some (map_ f' f' f t)
  with Exit -> None
let op2 check f t1 t2 =
  let f' t1 t2 = if check t1 t2 then f t1 t2 else raise Exit in
  try Some (map2_ f' f' f t1 t2)
  with Exit -> None
let opl check f ts =
  let f' ts = if check ts then f ts else raise Exit in
  try Some (mapl_ f' f' f ts)
  with Exit -> None
let cup = map2 Ty.cup
let cap = map2 Ty.cap
let disj = List.fold_left cup empty
let conj = List.fold_left cap any
let neg t =
  if t.eq then
    Ty.neg t.lb |> mk
  else
    { lb=Ty.neg t.ub ; ub=Ty.neg t.lb ; eq=false }

let fv t =
  if t.eq then TVOp.vars t.lb else MVarSet.union (TVOp.vars t.lb) (TVOp.vars t.ub)
let substitute s = map (Subst.apply s)

(* let test f t =
  if t.eq then f t.lb else (f t.lb) && (f t.ub) *)
let test2 f t1 t2 =
  if t1.eq && t2.eq then
    f t1.lb t2.lb
  else
    (f t1.lb t2.lb) && (f t1.ub t2.ub)
let is_empty { ub ; _ } = Ty.is_empty ub
let is_any { lb ; _ } = Ty.is_any lb
let leq = test2 Ty.leq
let equiv = test2 Ty.equiv
let non_gradual { eq ; _ } = eq

let simplify = map Ty.simplify
let normalize = map Ty.normalize

let pp' s fmt t =
  let pp = TVOp.pp_typ_subst s in
  if t.eq then
    Format.fprintf fmt "%a" pp t.lb
  else
    let dynv = Sstt.Var.mk (Format.asprintf "%a" PrinterCfg.print_dyn ()) in
    let ty = Ty.cap (Sstt.Ty.mk_var dynv) t.ub |> Ty.cup t.lb in
    Format.fprintf fmt "%a" pp ty
let pp fmt t = pp' Subst.identity fmt t

let mk_gradual lb ub =
  if Ty.leq lb ub |> not
  then raise (Invalid_argument "Upper bound must be larger than lower bound") ;
  mk_gradual lb ub

module Builder = struct
  let dynvars = ref TVarSet.empty
  let dyn () =
    let v = Sstt.Var.mk (Format.asprintf "%a" PrinterCfg.print_dyn ()) in
    dynvars := TVarSet.add v !dynvars ;
    Sstt.Ty.mk_var v
  let dynvars_of_ty ty =
    Sstt.Ty.vars ty |> TVarSet.inter !dynvars
  let non_gradual ty =
    dynvars_of_ty ty |> TVarSet.is_empty
  let refresh ty =
    let s = dynvars_of_ty ty |> TVarSet.elements
    |> List.map (fun v -> v, dyn ()) |> Subst.of_list1 in
    Subst.apply s ty
  let build ty =
    let tvs = dynvars_of_ty ty in
    let sub, slb = tvs |> TVarSet.elements |> List.map (fun v ->
      match TVOp.polarity1 v ty with
      | `None -> assert false
      | `Both -> invalid_arg "Dyn occurs in an invariant position."
      | `Pos -> (v, Ty.any), (v, Ty.empty)
      | `Neg -> (v, Ty.empty), (v, Ty.any)
    ) |> List.split in
    let ub = Subst.apply (Subst.of_list1 sub) ty in
    let lb = Subst.apply (Subst.of_list1 slb) ty in
    mk_gradual lb ub
end