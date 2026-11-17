open Mlsem_common
open Mlsem_types
module SA = Mlsem_system.Ast

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
let replace_in_env v ty env =
  add_to_env v ty (Env.rm v env)

let a = TVar.mk KInfer None
let aty v =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some None -> TVar.typ a |> GTy.mk
  | Some (Some gty) -> gty
let aref_codom v =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some None -> TVar.typ a |> mk_ref |> GTy.mk
  | Some (Some gty) -> gty
  (* The type of an AnnotMut variable must be kept in the environment
     so that its type variables stay bound to it (and thus cannot be generalized). *)
let aref_dom v =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some None -> TVar.typ a |> mk_ref
  | Some (Some _) -> Ty.any

let quantified = MVarSet.of_list [a] []
let mk dom codom = GTy.map (Arrow.mk dom) codom |> TyScheme.mk quantified

let ref_uninit v = mk Ty.unit (aref_codom v)
let ref_uninit v =
  let arg = Eid.unique (), SA.Value (GTy.mk Ty.unit) in
  let op = { SA.oname="ref_uninit" ; ofun=Fun.const (ref_uninit v) ; ogen=false } in
  SA.Operation (OCustom op, arg)

let ref_cons v = mk (aty v |> GTy.ub) (aref_codom v)
let ref_cons v arg =
  let arg = Eid.unique (), SA.GConstructor (SA.UpperBound, [arg]) in
  let op = { SA.oname="ref" ; ofun=Fun.const (ref_cons v) ; ogen=false } in
  SA.Operation (OCustom op, arg)

let ref_get v = mk (aref_dom v) (aty v)
let ref_get v =
  let arg = Eid.unique (), SA.Var v in
  let op = { SA.oname="get" ; ofun=Fun.const (ref_get v) ; ogen=false } in
  SA.Operation (OCustom op, arg)

let ref_assign v =
  mk (Tuple.mk [aref_dom v ; aty v |> GTy.ub]) (!Config.void_ty |> GTy.mk)
let ref_assign v arg =
  let arg = Eid.unique (), SA.GConstructor (SA.UpperBound, [arg]) in
  let arg = Eid.unique (), SA.Constructor (SA.Tuple 2,[
          (Eid.unique (), SA.Var v) ; arg
      ]) in
  let op = { SA.oname="assign" ; ofun=Fun.const (ref_assign v) ; ogen=false } in
  SA.Operation (OCustom op, arg)

let as_ref_assign (e:SA.e) =
  match e with
  | SA.Operation (OCustom { oname="assign" ; _ },
      (_, SA.Constructor (SA.Tuple 2, [ (_, SA.Var v) ; (_, SA.GConstructor (SA.UpperBound, [_])) ])))
    when is_mutable v -> Some v
  | _ -> None

let content_ty v ref_ty =
  match Hashtbl.find_opt all v with
  | None -> invalid_arg "Variable must be mutable."
  | Some (Some gty) -> Some (GTy.ub gty)
  | Some None ->
      (* The type of a cell is obtained by substitution from [ref('a)]:
        we only handle the case of a single [ref] atom. *)
      if Ty.leq ref_ty (Abstract.any ref_abs) |> not then None
      else
        match Abstract.dnf ref_abs ref_ty with
        | [[[ty]]] -> Some ty
        | _ -> None
