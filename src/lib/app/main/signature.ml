open Mlsem_types
open Mlsem_common

type overload = GTy.t
type t = overload list

let is_well_formed ty =
  GTy.fv ty
  |> MVarSet.filter
    (fun tv -> TVar.has_kind KNoInfer tv |> not)
    (fun rv -> RVar.has_kind KNoInfer rv |> not)
  |> MVarSet.is_empty

let simplify ty = ty |> TyScheme.bot_instance |> TyScheme.norm_and_simpl

let to_tyscheme env tys =
  GTy.conj tys |> TyScheme.mk_poly_except (Env.tvars env) |> simplify

let extract ty =
  let tvs, ty = TyScheme.get ty in
  Subst.apply (TVOp.shorten_names tvs) (GTy.ub ty)
let rec decompose ty =
  if Ty.leq ty Arrow.any then
    match Arrow.dnf ty with
    | [arrs] -> arrs |> List.concat_map
      (fun (a,b) -> decompose b |> List.map (fun b -> Arrow.mk a b))
    | _ -> [ty]
  else [ty]
let of_ty ty = ty |> decompose |> List.map GTy.mk
let of_tyscheme ty = ty |> extract |> of_ty

let merge tys =
  tys |> GTy.mapl (fun tys ->
      if List.for_all (fun ty -> Ty.leq ty Arrow.any) tys
      then
        let dom = List.map Arrow.domain tys |> Ty.disj in
        let codom = Arrow.apply (Ty.conj tys) dom in
        Arrow.mk dom codom
      else Ty.conj tys
    )
