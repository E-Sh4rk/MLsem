open Common
open Types

type t = Variable.t

let all = Hashtbl.create 100
let create name =
  let v = Variable.create_let name in
  Hashtbl.add all v () ; v

let is_mutable v = Hashtbl.mem all v

let ref_abs = Abstract.define "ref" 1
let mk_ref ty = Abstract.mk ref_abs [ty]

let add_to_env env v ty =
  if TVOp.vars ty |> TVarSet.is_empty |> not
    then invalid_arg "Type must not contain type variables." ;
  Env.add v (mk_ref ty |> GTy.mk |> TyScheme.mk_mono) env

let ref_cons () =
  let a = TVar.mk TVar.KInfer None in
  Arrow.mk (TVar.typ a) (TVar.typ a |> mk_ref)

let ref_get () =
  let a = TVar.mk TVar.KInfer None in
  Arrow.mk (TVar.typ a |> mk_ref) (TVar.typ a)

let ref_assign () =
  let a = TVar.mk TVar.KInfer None in
  Arrow.mk (Tuple.mk [TVar.typ a |> mk_ref ; TVar.typ a]) (!System.Config.void_ty)
