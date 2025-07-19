open Base
open Tvar

module GTy = struct
  type t = { lb:typ ; ub:typ ; eq:bool }
  let mk ty = { lb=ty ; ub=ty ; eq=true }
  let mk_gradual lb ub = { lb ; ub ; eq=subtype ub lb }
  let empty = mk empty
  let any = mk any
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
  let op f t =
    let flb t = match f t with None -> raise Exit | Some t -> t in
    let fub t = match f t with None -> Base.any | Some t -> t in
    try Some (map_ flb flb fub t)
    with Exit -> None
  let op2 f t1 t2 =
    let flb t1 t2 = match f t1 t2 with None -> raise Exit | Some t -> t in
    let fub t1 t2 = match f t1 t2 with None -> Base.any | Some t -> t in
    try Some (map2_ flb flb fub t1 t2)
    with Exit -> None
  let cup = map2 cup
  let cap = map2 cap
  let disj = List.fold_left cup empty
  let conj = List.fold_left cap any
  let neg t =
    if t.eq then
      neg t.lb |> mk
    else
      { lb=neg t.ub ; ub=neg t.lb ; eq=false }

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
  let is_empty = test is_empty
  let is_any = test is_any
  let leq = test2 subtype
  let equiv = test2 equiv

  let simplify = map Additions.simplify_typ
  let normalize = map normalize

  let pp fmt t =
    if t.eq then
      Format.fprintf fmt "%a" pp_typ t.lb
    else
      let lb,ub = Base.is_empty t.lb, Base.is_any t.ub in
      if lb && ub then
        Format.fprintf fmt ".."
      else if lb then
        Format.fprintf fmt "..(%a)" pp_typ t.ub
      else if ub then
        Format.fprintf fmt "(%a).." pp_typ t.lb
      else
        Format.fprintf fmt "(%a)..(%a)" pp_typ t.lb pp_typ t.ub

  let mk_gradual lb ub =
    assert (subtype lb ub) ;
    mk_gradual lb ub
end
