open Tvar
open Gradual

module TyScheme = struct
  type t = TVarSet.t * GTy.t
  let mk tvs ty =
    let tvs = TVarSet.inter tvs (GTy.fv ty) in
    (tvs, ty)
  let mk_poly_except mono ty =
    let tvs = TVarSet.diff (GTy.fv ty) mono in
    (tvs, ty)
  let mk_mono ty = mk TVarSet.empty ty
  let mk_poly ty = mk_poly_except TVarSet.empty ty
  let get (tvs, ty) = (tvs, ty)
  let get_fresh (tvs, ty) =
    let mono = TVarSet.diff (GTy.fv ty) tvs in
    let s = refresh tvs in
    mono, GTy.substitute s ty
  let fv (tvs, ty) = TVarSet.diff (GTy.fv ty) tvs

  let substitute s (tvs, ty) =
    let s = Subst.remove s tvs in
    (tvs, GTy.substitute s ty)

  let leq (tvs1,ty1) (tvs2,ty2) =
    TVarSet.subset tvs2 tvs1 &&
    GTy.leq ty1 ty2
  let equiv t1 t2 = leq t1 t2 && leq t2 t1
  let bot_instance (tvs,t) =
    let mono = TVarSet.diff (GTy.fv t) tvs in
    let t = GTy.map (bot_instance mono) t in
    mk tvs t
  let top_instance (tvs,t) =
    let mono = TVarSet.diff (GTy.fv t) tvs in
    let t = GTy.map (top_instance mono) t in
    mk tvs t
  let simplify (tvs,ty) = (tvs, GTy.simplify ty)
  let normalize (tvs,ty) = (tvs, GTy.normalize ty)
  let norm_and_simpl ts = ts |> normalize |> simplify
  let pp fmt (tvs, ty) =
    if TVarSet.is_empty tvs
    then Format.fprintf fmt "%a" GTy.pp ty
    else
      Format.fprintf fmt "∀%a. %a"
        (Utils.pp_seq TVar.pp ",") (TVarSet.destruct tvs) GTy.pp ty
  let pp_short fmt (tvs, ty) =
    let s = shorten_names tvs in
    let ty = GTy.substitute s ty in
    let tvs = TVarSet.destruct tvs
      |> List.map (fun v -> Subst.find s v |> vars |> TVarSet.destruct |> List.hd)
      |> TVarSet.construct
    in
    pp fmt (tvs,ty)
end
