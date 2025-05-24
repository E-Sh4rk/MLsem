open Base
open Tvar

type t = TVarSet.t * typ
let mk tvs ty =
  let tvs = TVarSet.inter tvs (vars ty) in
  (tvs, ty)
let mk_poly_except mono ty =
  let tvs = TVarSet.diff (vars ty) mono in
  (tvs, ty)
let mk_mono ty = mk TVarSet.empty ty
let mk_poly ty = mk (vars ty) ty
let get (tvs, ty) = (tvs, ty)
let get_fresh (tvs, ty) =
  let mono = TVarSet.diff (vars ty) tvs in
  let s = refresh tvs in
  mono, Subst.apply s ty
let fv (tvs, ty) = TVarSet.diff (vars ty) tvs

let substitute s (tvs, ty) =
  let s = Subst.remove s tvs in
  (tvs, Subst.apply s ty)

let leq (tvs1,ty1) (tvs2,ty2) =
  TVarSet.subset tvs2 tvs1 &&
  subtype ty1 ty2
let equiv t1 t2 = leq t1 t2 && leq t2 t1
let bot_instance (tvs,t) =
  let mono = TVarSet.diff (vars t) tvs in
  let t = Additions.bot_instance mono t in
  mk tvs t
let top_instance (tvs,t) =
  let mono = TVarSet.diff (vars t) tvs in
  let t = Additions.top_instance mono t in
  mk tvs t
let leq_inst t1 ty2 =
  let t1 = bot_instance t1 in
  let mono1, ty1 = get_fresh t1 in
  let mono = TVarSet.union mono1 (vars ty2) in
  test_tallying mono [ty1,ty2]
let geq_inst t1 ty2 =
  let t1 = top_instance t1 in
  let mono1, ty1 = get_fresh t1 in
  let mono = TVarSet.union mono1 (vars ty2) in
  test_tallying mono [ty2,ty1]
let simplify (tvs,ty) = (tvs, Additions.simplify_typ ty)
let pp fmt (tvs, ty) =
  if TVarSet.is_empty tvs
  then Format.fprintf fmt "%a" pp_typ ty
  else
    Format.fprintf fmt "∀%a. %a"
      (Utils.pp_seq TVar.pp ",") (TVarSet.destruct tvs) pp_typ ty
let pp_short fmt (tvs, ty) =
  let s = shorten_names tvs in
  let ty = Subst.apply s ty in
  let tvs = TVarSet.destruct tvs
    |> List.map (fun v -> Subst.find s v |> vars |> TVarSet.destruct |> List.hd)
    |> TVarSet.construct
  in
  pp fmt (tvs,ty)
