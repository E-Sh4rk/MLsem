open Types.Base
open Types
open Parsing.Variable
open Env

type t = REnv.t list
[@@deriving show]

let empty = []
let add (_,_,r) t = r::t (* TODO *)

let env_to_typ renv =
  let bindings = renv
    |> REnv.bindings |> List.map
      (fun (v, ty) -> (Variable.get_unique_name v, (false, ty)))
  in
  mk_record true bindings

let covers mono t (_,_,renv) = (* TODO *)
  let a = t |> List.map env_to_typ |> disj in
  let a = TyScheme.mk_poly_except mono a in
  let b = env_to_typ renv in
  let b = TyScheme.mk_poly_except mono b in
  TyScheme.geq_inst a b
