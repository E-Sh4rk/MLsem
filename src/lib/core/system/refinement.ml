open Mlsem_common
open Ast
open Mlsem_types
open TVOp
open Mlsem_utils

let rec is_undesirable_arrow s =
  Ty.leq s Arrow.any &&
  Arrow.dnf s |> List.for_all
    (List.exists (fun (a, b) -> Ty.non_empty a && is_undesirable_arrow b))

let is_undesirable mono s =
  TVarSet.subset (vars s) mono |> not ||
  is_undesirable_arrow s

let combine rs1 rs2 =
  Utils.carthesian_prod rs1 rs2
  |> List.map (fun (r1, r2) -> REnv.cap r1 r2)

let combine' rss =
  Utils.carthesian_prod' rss |> List.map REnv.conj

let sufficient_refinements env e t =
  let rec aux (_,e) t =
    if Ty.is_any t then [REnv.empty] else
    match e with
    | Lambda _ -> []
    | LambdaRec _ -> []
    | Var v -> [REnv.singleton v t]
    | Constructor (c, es) ->
      Checker.domains_of_construct c t
      |> List.concat_map (fun ts ->
        List.map2 (fun e t -> aux e t) es ts |> combine')
    | TypeCoerce (_, s, _) when Ty.leq (GTy.lb s) t -> [REnv.empty]
    | Value s when Ty.leq (GTy.lb s) t -> [REnv.empty]
    | Value _ | TypeCoerce _ -> []
    | Projection (p, e) -> aux e (Checker.domain_of_proj p t)
    | TypeCast (e, _) -> aux e t
    | App ((_, Var v), e) when Env.mem v env ->
      let alpha = TVar.mk KInfer None in
      let (mono, ty) = Env.find v env |> TyScheme.get_fresh in
      let mono = TVarSet.union mono (vars t) in
      begin match Arrow.dnf (GTy.lb ty) with
      | [] -> []
      | [arrows] ->
        let t1 = Arrow.of_dnf [arrows] in
        let res = tallying mono [ (t1, Arrow.mk (TVar.typ alpha) t) ] in
        res |> List.concat_map (fun sol ->
            let targ = Subst.find sol alpha |> top_instance mono in
            if is_undesirable mono targ then [] else aux e targ
          )
      | _ -> []
      end
    | App _ -> []
    | Ite (e, s, e1, e2) ->
      let r1 = combine (aux e s) (aux e1 t) in
      let r2 = combine (aux e (Ty.neg s)) (aux e2 t) in
      r1@r2
    | Let (_, _, _, _) -> []
  in
  aux e t

let refine env e t =
  let base_renv = REnv.empty in
  let renvs = sufficient_refinements env e (Ty.neg t) in
  let rec aux renv renvs =
    let renvs = renvs |> List.map (fun renv' ->
      renv' |> REnv.filter (fun v ty ->
        let _, ty' = Env.find v env |> TyScheme.get in
        let ty'' = REnv.find' v renv in
        Ty.leq (Ty.cap (GTy.lb ty') ty'') ty |> not
      )
    )
    in
    let renv' = REnv.cap renv (List.filter_map REnv.neg_approx renvs |> REnv.conj) in
    if REnv.leq renv renv' then renv else aux renv' renvs
  in
  aux base_renv renvs

let rec typeof env (_,e) =
  match e with
  | Var v when Env.mem v env -> Env.find v env
  (* These cases are necessary because of pattern matching encoding *)
  | Projection (p, t) ->
    let _, ty = typeof env t |> TyScheme.get in
    TyScheme.mk_mono (GTy.map (Checker.proj p) ty)
  | TypeCast (t, _) -> typeof env t
  | TypeCoerce (_, ty, _) -> TyScheme.mk_mono ty
  | _ -> TyScheme.mk_mono GTy.any

let refinement_envs env e =
  let res = ref REnvSet.empty in
  let add_refinement env e t =
    res := REnvSet.add !res (refine env e t)
  in
  let rec aux_lambda env (d,v,e) =
    let t = TyScheme.mk_mono d in
    aux (Env.add v t env) e
  and aux env (_,e) : unit =
    match e with
    | Value _ | Var _ -> ()
    | Constructor (_, es) -> es |> List.iter (aux env)
    | Projection (_, e) | TypeCast (e, _) | TypeCoerce (e, _, _) -> aux env e
    | Lambda (d, v, e) -> aux_lambda env (d,v,e)
    | LambdaRec lst -> lst |> List.iter (aux_lambda env)
    | Ite (e, tau, e1, e2) ->
      if fv e1 |> VarSet.is_empty |> not then add_refinement env e tau ;
      if fv e2 |> VarSet.is_empty |> not then add_refinement env e (Ty.neg tau) ;
      aux env e ; aux env e1 ; aux env e2
    | App (e1, e2) -> aux env e1 ; aux env e2
    | Let (_, v, e1, e2) ->
      aux env e1 ; aux (Env.add v (typeof env e1) env) e2 ;
      let res' =
        REnvSet.elements !res |> List.map (fun renv ->
          if REnv.mem v renv
          then
            let renv' = refine env e1 (REnv.find v renv) in
            REnv.cap renv renv'
          else renv
        ) |> REnvSet.of_list
      in
      res := res'
  in
  aux env e ; !res

let partition ts =
  let cap_if_nonempty t t' =
    let s = Ty.cap t t' in
    if Ty.is_empty s then t else s
  in
  let rec aux t =
    if Ty.is_empty t then []
    else
      let s = List.fold_left cap_if_nonempty t ts in
      s::(aux (Ty.diff t s))
  in
  aux Ty.any

module Partitioner = struct
  type t = REnv.t list

  let isolate_tuple_comp (n,lst) =
    lst |>
    List.filter (function [] -> false | _ -> true) |>
    List.map (fun atom -> n, [atom])
  let isolate_tuple_conjuncts t =
    let (comps, _) = Tuple.decompose t in
    let comps = comps |> List.concat_map (fun cp -> isolate_tuple_comp cp) in
    let comps = comps |> List.map (fun cp -> Tuple.recompose ([cp], false)) in
    comps
  let isolate_record_conjuncts t =
    Record.dnf t |>
    List.filter (function [], _ -> false | _, _ -> true) |>
    List.map (fun atom -> Record.of_dnf [atom])
  let isolate_conjuncts t =
    (* Necessary because of pattern matching encoding for uncurrified functions *)
    t::(isolate_tuple_conjuncts t)@(isolate_record_conjuncts t)

  let from_renvset rs = REnvSet.elements rs
  let filter_compatible lst v ty =
    lst |> List.filter (fun renv ->
      (REnv.mem v renv |> not) ||
      (Ty.disjoint ty (REnv.find v renv) |> not)
    )
  let partition_for t v extra =
    let tys = t |> List.filter_map (fun renv ->
      if REnv.mem v renv then Some (REnv.find v renv) else None
    ) |> partition |> List.concat_map isolate_conjuncts in
    extra@tys |> partition
    (* |> (fun tys -> Format.printf "Partition for %a: %a@." Variable.pp v
      (Utils.pp_list Ty.pp) tys ; tys) *)
end
