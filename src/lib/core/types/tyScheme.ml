open Tvar
open Mlsem_utils

type t = MVarSet.t * GTy.t
let mk tvs ty =
  let tvs = MVarSet.inter tvs (GTy.fv ty) in
  (tvs, ty)
let mk_poly_except mono ty =
  let tvs = MVarSet.diff (GTy.fv ty) mono in
  (tvs, ty)
let mk_mono ty = mk MVarSet.empty ty
let mk_poly ty = mk_poly_except MVarSet.empty ty
let get (tvs, ty) = (tvs, ty)
let get_fresh (tvs, ty) =
  let mono = MVarSet.diff (GTy.fv ty) tvs in
  let s = TVOp.refresh ~kind:KInfer tvs in
  mono, GTy.substitute s ty
let fv (tvs, ty) = MVarSet.diff (GTy.fv ty) tvs

let substitute s (tvs, ty) =
  let s = Subst.remove_many tvs s in
  (tvs, GTy.substitute s ty)

let leq (tvs1,ty1) (tvs2,ty2) =
  MVarSet.subset (GTy.fv ty1 |> MVarSet.inter tvs2) tvs1 &&
  GTy.leq ty1 ty2
let equiv t1 t2 = leq t1 t2 && leq t2 t1
let bot_instance (tvs,t) =
  let mono = MVarSet.diff (GTy.fv t) tvs in
  let t = GTy.map (TVOp.bot_instance mono) t in
  mk tvs t
let top_instance (tvs,t) =
  let mono = MVarSet.diff (GTy.fv t) tvs in
  let t = GTy.map (TVOp.top_instance mono) t in
  mk tvs t
let simplify (tvs,ty) = (tvs, GTy.simplify ty)
let normalize (tvs,ty) = (tvs, GTy.normalize ty)
let norm_and_simpl ts = ts |> normalize |> simplify
let pp' s fmt (vs, ty) =
  if MVarSet.is_empty vs
  then Format.fprintf fmt "%a" (GTy.pp' s) ty
  else
    let pp_mvar fmt v =
      match v with `T v -> TVar.pp fmt v | `R v -> RVar.pp fmt v
    in
    let tvs, rvs = MVarSet.elements vs in
    let tvs = tvs |> List.concat_map (fun v -> Subst.find1 s v |> TVOp.vars |> MVarSet.elements1) in
    let rvs = rvs |> List.concat_map (fun v -> Subst.find2 s v |> Row.all_vars |> MVarSet.elements2) in
    let vs = (List.map (fun v -> `T v) tvs)@(List.map (fun v -> `R v) rvs) in
    Format.fprintf fmt "∀%a. %a" (Utils.pp_seq pp_mvar ",") vs (GTy.pp' s) ty
let pp_short fmt (tvs, ty) =
  let s = TVOp.shorten_names tvs in
  pp' s fmt (tvs,ty)
let pp fmt t = pp' Subst.identity fmt t
