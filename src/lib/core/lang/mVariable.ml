open Common
open Types

type t = Variable.t
type kind = Immut | AnnotMut of Ty.t | Mut

let all = Hashtbl.create 100

let add_to_tbl v kind =
  match kind with
  | Immut -> ()
  | Mut -> Hashtbl.add all v None
  | AnnotMut ty -> Hashtbl.add all v (Some ty)

let create_let kind name =
  let v = Variable.create_let name in
  add_to_tbl v kind ; v
let create_gen kind name =
  let v = Variable.create_gen name in
  add_to_tbl v kind ; v
let create_lambda kind name =
  let v = Variable.create_lambda name in
  add_to_tbl v kind ; v

let is_mutable v = Hashtbl.mem all v
let kind v =
  match Hashtbl.find_opt all v with
  | None -> Immut
  | Some None -> Mut
  | Some (Some ty) -> AnnotMut ty

let kind_equal k1 k2 =
  match k1, k2 with
  | Immut, Immut -> true
  | Mut, Mut -> true
  | AnnotMut ty1, AnnotMut ty2 when Ty.equiv ty1 ty2 -> true
  | _, _ -> false

let ref_abs = Abstract.define "ref" 1
let mk_ref ty = Abstract.mk ref_abs [ty]

let add_to_env env v =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some None -> invalid_arg "Variable must be annotated."
  | Some (Some ty) -> Env.add v (mk_ref ty |> GTy.mk |> TyScheme.mk_mono) env

let subst_if_ann v a ty =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some None -> ty
  | Some (Some ty') -> Subst.apply (Subst.construct [a,ty']) ty

let ref_cons v =
  let a = TVar.mk TVar.KInfer None in
  Arrow.mk (TVar.typ a) (TVar.typ a |> mk_ref)
  |> subst_if_ann v a

let ref_get v =
  let a = TVar.mk TVar.KInfer None in
  Arrow.mk (TVar.typ a |> mk_ref) (TVar.typ a)
  |> subst_if_ann v a

let ref_assign v =
  let a = TVar.mk TVar.KInfer None in
  Arrow.mk (Tuple.mk [TVar.typ a |> mk_ref ; TVar.typ a]) (!System.Config.void_ty)
  |> subst_if_ann v a
