open Mlsem_common
open Ast
open Mlsem_types
open TVOp
open Mlsem_utils

let rec typeof env (_,e) =
  match e with
  | TypeCoerce (_, ty, _) -> ty
  (* The cases below are necessary because of pattern matching encoding *)
  | Var v when Env.mem v env -> Env.find v env |> TyScheme.get_fresh |> snd
  | Projection (p, e) -> GTy.map (Checker.proj p) (typeof env e )
  | TypeCast (e, ty, _) -> GTy.cap (typeof env e) ty
  | _ -> GTy.any
let typeof_def env e = Checker.generalize ~e env (typeof env e)

let combine rs1 rs2 =
  Utils.carthesian_prod rs1 rs2
  |> List.map (fun (r1, r2) -> REnv.cap r1 r2)
let combine' rss =
  Utils.carthesian_prod' rss |> List.map REnv.conj

let is_empty renv =
  REnv.bindings renv |> List.exists (fun (_,ty) -> Ty.is_empty ty)

let sufficient_refinements env e t =
  let rec aux env (_,e) t =
    let app t1 e2 =
      let alpha = TVar.mk KInfer None in
      let (mono, ty) = TyScheme.get_fresh t1 in
      let mono = MVarSet.union mono (vars t) in
      begin match Arrow.dnf (GTy.lb ty) with
      | [] -> []
      | [arrows] ->
        let t1 = Arrow.of_dnf [arrows] in
        let res = tallying mono [ (t1, Arrow.mk (TVar.typ alpha) t) ] in
        res |> List.concat_map (fun sol ->
            let targ = Subst.find1 sol alpha |> top_instance mono in
            if MVarSet.subset (vars targ) mono |> not then [] else aux env e2 targ
          )
      | _ -> []
      end
    in
    let renvs = match e with
    | Lambda _ -> []
    | LambdaRec _ -> []
    | Var v -> [REnv.singleton v t]
    | Constructor (c, es) ->
      Checker.domains_of_construct c t |> List.concat_map
        (fun ts -> List.map2 (fun e t -> aux env e t) es ts |> combine')
    | TypeCoerce (_, s, _) when Ty.leq (GTy.lb s) t -> [REnv.empty]
    | Value s when Ty.leq (GTy.lb s) t -> [REnv.empty]
    | Value _ | TypeCoerce _ -> []
    | Projection (p, e) -> aux env e (Checker.domain_of_proj p t)
    | TypeCast (e, _, _) -> aux env e t
    | App ((_, Var v), e) when Env.mem v env -> app (Env.find v env) e
    | App _ -> []
    | Operation (o, e) -> app (Checker.fun_of_operation o) e
    | Ite (e, s, e1, e2) ->
      let s = GTy.lb s in
      let r1 = combine (aux env e s) (aux env e1 t) in
      let r2 = combine (aux env e (Ty.neg s)) (aux env e2 t) in
      r1@r2
    | Alt (e1, e2) -> (aux env e1 t)@(aux env e2 t)
    | Let (_, v, e1, e2) ->
      aux (Env.add v (typeof_def env e1) env) e2 t
      |> List.concat_map (fun renv ->
          let renv, t = REnv.rm v renv, REnv.find' v renv in
          let renvs = aux env e1 t in
          List.map (REnv.cap renv) renvs
        )
    in
    renvs |> List.filter (fun renv -> is_empty renv |> not)
  in
  aux env e t

let refine env e t =
  let base_renv = REnv.empty in
  let renvs = sufficient_refinements env e (Ty.neg t) in
  let rec aux renv renvs =
    let renvs = renvs |> List.map (fun renv' ->
      renv' |> REnv.filter (fun v ty ->
        let ty' = Env.find v env |> TyScheme.get_fresh |> snd |> GTy.lb in
        let ty'' = REnv.find' v renv in
        Ty.leq (Ty.cap ty' ty'') ty |> not
      )
    )
    in
    let renv' = REnv.cap renv (List.filter_map REnv.neg_approx renvs |> REnv.conj) in
    if REnv.leq renv renv' then renv else aux renv' renvs
  in
  aux base_renv renvs

let refinement_envs
  ?(extra_checks=[]) ?(refine_on_typecases=true) ?(refine_on_casts=false) env e =
  let extra = Hashtbl.create 10 in
  extra_checks |> List.iter (fun (eid, ty) -> Hashtbl.add extra eid ty) ;
  let res = ref REnvSet.empty in
  let add_refinement env e t =
    res := REnvSet.add !res (refine env e t)
  in
  let rec aux_lambda env (d,v,e) =
    let t = TyScheme.mk_mono d in
    aux (Env.add v t env) e
  and aux env (id,e) : unit =
    let extra = Hashtbl.find_all extra id in
    extra |> List.iter (fun ty -> add_refinement env (id,e) ty) ;
    match e with
    | Value _ | Var _ -> ()
    | Constructor (_, es) -> es |> List.iter (aux env)
    | Projection (_, e) | TypeCoerce (e, _, _) | Operation (_, e) -> aux env e
    | Lambda (d, v, e) -> aux_lambda env (d,v,e)
    | LambdaRec lst -> lst |> List.iter (aux_lambda env)
    | TypeCast (e, tau, _) ->
      if refine_on_casts then add_refinement env e (GTy.lb tau) ;
      aux env e
    | Ite (e, tau, e1, e2) ->
      if refine_on_typecases then begin
        let tau = GTy.lb tau in
        if fv e1 |> VarSet.is_empty |> not then add_refinement env e tau ;
        if fv e2 |> VarSet.is_empty |> not then add_refinement env e (Ty.neg tau)
      end ;
      aux env e ; aux env e1 ; aux env e2
    | App (e1, e2) | Alt (e1, e2) -> aux env e1 ; aux env e2
    | Let (_, v, e1, e2) ->
      aux env e1 ; aux (Env.add v (typeof_def env e1) env) e2 ;
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
