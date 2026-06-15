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

let rec decompose ty =
  if Ty.leq ty Arrow.any then
    match Arrow.dnf ty with
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

(* The variable's name as it is rendered in the overload's text (the printer
   uses the var's full prefixed name), so combobox labels match what the user
   sees and [instantiate] can select by that same string. *)
let var_name v = TVar.prefix v ^ TVar.name v

(* The (prefixed) names of [overload]'s free type variables. Variables are
   per-overload: two overloads each rendering ['a] are distinct vars, so this
   is always called per single overload. *)
let overload_vars overload =
  TVOp.vars overload |> MVarSet.proj1 |> TVarSet.elements |> List.map var_name

(* Substitute every type variable of [overload] named [var_name] with the
   concrete type [ty]. *)
let instantiate overload name ty =
  let vs =
    TVOp.vars overload |> MVarSet.proj1 |> TVarSet.filter (fun v -> var_name v = name)
  in
  let s = vs |> TVarSet.elements |> List.map (fun v -> (v, ty)) |> Subst.of_list1 in
  Subst.apply s overload

let rec merge tys =
  if tys <> []
     && List.for_all (fun ty -> Ty.leq ty Arrow.any) tys
     && not (List.exists Ty.is_empty tys)
  then
    let dom = List.map Arrow.domain tys |> Ty.disj in
    let codom = Arrow.apply (Ty.conj tys) dom |> decompose |> merge in
    Arrow.mk dom codom
  else Ty.conj tys

let pp_overload fmt o = GTy.Builder.pp fmt o
