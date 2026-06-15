open Mlsem_types
open Mlsem_common

type overload = Ty.t (* Unbuilt gradual type *)
type t = overload list

let to_gty overload = GTy.Builder.build overload

let is_well_formed overload =
  overload |> to_gty |> GTy.fv
  |> MVarSet.filter
    (fun tv -> TVar.has_kind KNoInfer tv |> not)
    (fun rv -> RVar.has_kind KNoInfer rv |> not)
  |> MVarSet.is_empty

let is_arrow_sig ty = Ty.leq ty Arrow.any && not (Ty.is_empty ty)

let rec decompose ty =
  if is_arrow_sig ty then
    match Arrow.dnf ty with
    | [[]] -> [Arrow.any]
    | [arrs] -> arrs |> List.concat_map
      (fun (a,b) -> decompose b |> List.map (fun b -> Arrow.mk a b))
    | _ -> [ty]
  else [ty]

let build benv ty =
  let open Builder in
  let ty, benv = type_expr_to_typ ~allow_gradual:true benv ty in
  if is_well_formed ty |> not then invalid_arg "Unresolved signature" ;
  decompose ty, benv

let simplify ty = ty |> TyScheme.bot_instance |> TyScheme.norm_and_simpl

let to_tyscheme env tys =
  tys |> List.map to_gty |> GTy.conj |> TyScheme.mk_poly_except (Env.tvars env) |> simplify

let extract ty =
  let tvs, ty = TyScheme.get ty in
  let ty = Subst.apply (TVOp.shorten_names ~kind:KNoInfer tvs) (GTy.ub ty) in
  if is_well_formed ty |> not then invalid_arg "Unresolved signature" ;
  ty
let of_tyscheme ty = ty |> extract |> decompose

let rec merge ty =
  if is_arrow_sig ty then
    let dom = Arrow.domain ty in
    let codom = Arrow.apply ty dom |> merge in
    Arrow.mk dom codom
  else ty
let merge tys = merge (Ty.conj tys)

let pp_overload fmt o = GTy.Builder.pp fmt o
