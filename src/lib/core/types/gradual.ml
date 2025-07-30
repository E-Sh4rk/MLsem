open Base
open Tvar

module GTy = struct
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
    if t.eq then vars t.lb else TVarSet.union (vars t.lb) (vars t.ub)
  let substitute s = map (Subst.apply s)

  let test f t =
    if t.eq then f t.lb else (f t.lb) && (f t.ub)
  let test2 f t1 t2 =
    if t1.eq && t2.eq then
      f t1.lb t2.lb
    else
      (f t1.lb t2.lb) && (f t1.ub t2.ub)
  let is_empty = test Ty.is_empty
  let is_any = test Ty.is_any
  let leq = test2 Ty.leq
  let equiv = test2 Ty.equiv

  let simplify = map Ty.simplify
  let normalize = map Ty.normalize

  let pp fmt t =
    if t.eq then
      Format.fprintf fmt "%a" Ty.pp t.lb
    else
      let lb,ub = Ty.is_empty t.lb, Ty.is_any t.ub in
      if lb && ub then
        Format.fprintf fmt "#"
      else if lb then
        Format.fprintf fmt "#(%a)" Ty.pp t.ub
      else if ub then
        Format.fprintf fmt "(%a) | #" Ty.pp t.lb
      else
        Format.fprintf fmt "(%a) | #(%a)" Ty.pp t.lb Ty.pp t.ub

  let mk_gradual lb ub =
    assert (Ty.leq lb ub) ;
    mk_gradual lb ub
end
