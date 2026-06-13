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

let rec of_ty ty =
  if Ty.leq ty Arrow.any then
    match Arrow.dnf ty with
    | [arrs] -> arrs |> List.concat_map
      (fun (a,b) -> of_ty b |> List.map (fun b -> Arrow.mk a b))
    | _ -> [ty]
  else [ty]
let of_ty ty = of_ty ty |> List.map GTy.mk

let merge tys =
  tys |> GTy.mapl (fun tys ->
      if List.for_all (fun ty -> Ty.leq ty Arrow.any) tys
      then
        let dom = List.map Arrow.domain tys |> Ty.disj in
        let codom = Arrow.apply (Ty.conj tys) dom in
        Arrow.mk dom codom
      else Ty.conj tys
    )
