open Types.Base
open Types
open Parsing.Variable
open Env
open Annot

type t = IAnnot.coverage list
[@@deriving show]

let empty = []
let add c t = c::t

let env_to_typ renv =
  let bindings = renv
    |> REnv.bindings |> List.map
      (fun (v, ty) -> (Variable.get_unique_name v, (false, ty)))
  in
  mk_record true bindings

let more_specific mono res1 res2 =
  match res1, res2 with
  | _, None -> true
  | None, _ -> false
  | Some (eid1,ty1), Some (eid2,ty2) when eid1=eid2 ->
    let t1 = TyScheme.mk_poly_except mono ty1 in
    let t2 = TyScheme.mk_poly_except mono ty2 in
    TyScheme.leq_inst t1 t2
  | Some _, Some _ -> false

let covers mono t (res,renv) =
  let renvs = t
    |> List.filter (fun (res',_) -> more_specific mono res' res)
    |> List.map (fun (_,renv) -> renv)
  in
  let a = renvs |> List.map env_to_typ |> disj in
  let a = TyScheme.mk_poly_except mono a in
  let b = env_to_typ renv in
  let b = TyScheme.mk_poly_except mono b in
  TyScheme.geq_inst a b
