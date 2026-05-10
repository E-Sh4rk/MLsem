open Mlsem_common
open Mlsem_types

type t = Variable.t
type kind = Immut | AnnotMut of GTy.t | Mut

let all = Hashtbl.create 100

let add_to_tbl v kind =
  match kind with
  | Immut -> ()
  | Mut -> Hashtbl.add all v None
  | AnnotMut ty -> Hashtbl.add all v (Some ty)

let create kind name =
  let v = Variable.create name in
  add_to_tbl v kind ; v

let refresh kind v =
  let v = Variable.refresh v in
  add_to_tbl v kind ; v

let is_mutable v = Hashtbl.mem all v
let kind v =
  match Hashtbl.find_opt all v with
  | None -> Immut
  | Some None -> Mut
  | Some (Some gty) -> AnnotMut gty

let kind_equal k1 k2 =
  match k1, k2 with
  | Immut, Immut -> true
  | Mut, Mut -> true
  | AnnotMut gty1, AnnotMut gty2 when GTy.equiv gty1 gty2 -> true
  | _, _ -> false
let kind_compat k1 k2 =
  match k1, k2 with
  | Immut, Immut -> true
  | Mut, Mut -> true
  | Mut, AnnotMut _ -> true
  | AnnotMut gty1, AnnotMut gty2 when GTy.equiv gty1 gty2 -> true
  | _, _ -> false

let ref_abs = Abstract.define "ref" 1
let mk_ref ty = Abstract.mk ref_abs [ty]

let add_to_env v ty env =
  match Hashtbl.find_opt all v with
  | None -> Env.add v ty env
  | Some None -> Env.add v (GTy.dyn |> TyScheme.mk_mono) env
  | Some (Some ty') ->
    if GTy.fv ty' |> MVarSet.is_empty |> not
    then invalid_arg "Top-level mutable variables should not contain type variables." ;
    if Ty.leq (TyScheme.get ty |> snd |> GTy.lb) (GTy.ub ty') |> not
    then invalid_arg "Top-level mutable variable has an incompatible type." ;
    Env.add v (TyScheme.mk_mono ty') env

let a = TVar.mk KInfer None
let alb v =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some None -> TVar.typ a
  | Some (Some gty) -> GTy.lb gty
let aub v =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some None -> TVar.typ a
  | Some (Some gty) -> GTy.ub gty
let aref v =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some None -> TVar.typ a |> mk_ref
  | Some (Some _) -> Ty.any
let mk_gradual lb ub = GTy.mk_gradual lb ub |> TyScheme.mk_poly
let mk ty = GTy.mk ty |> TyScheme.mk_poly

let ref_uninit v =
  Arrow.mk Ty.unit (aref v) |> mk

let ref_cons v =
  Arrow.mk (aub v) (aref v) |> mk

let ref_get v =
  let lb = Arrow.mk (aref v) (alb v) in
  let ub = Arrow.mk (aref v) (aub v) in
  mk_gradual lb ub

let ref_assign v =
  Arrow.mk (Tuple.mk [aref v ; aub v]) (!Config.void_ty) |> mk
