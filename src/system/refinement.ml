open Ast
open Env
open Types
open Types.Base
open Types.Tvar
open Types.Gradual
open Types.Additions
open Utils

let rec is_undesirable_arrow s =
  subtype s arrow_any &&
  dnf s |> List.for_all
    (List.exists (fun (a, b) -> non_empty a && is_undesirable_arrow b))

let is_undesirable mono s =
  TVarSet.subset (vars s) mono |> not ||
  is_undesirable_arrow s

let combine rs1 rs2 =
  carthesian_prod rs1 rs2
  |> List.map (fun (r1, r2) -> REnv.cap r1 r2)

let sufficient_refinements env e t =
  let rec aux (_,e) t =
    if is_any t then [REnv.empty] else
    match e with
    | Lambda _ -> []
    | LambdaRec _ -> []
    | Var v -> [REnv.singleton v t]
    | Constructor (Tuple n, es) when List.length es = n ->
      tuple_dnf n t
      |> List.filter (fun b -> subtype (tuple_branch_type b) t)
      |> List.map (fun ts ->
        List.map2 (fun e t -> aux e t) es ts
        |> carthesian_prod' |> List.map REnv.conj
      ) |> List.flatten
    | Constructor (Cons, [e1;e2]) ->
      cons_dnf t
      |> List.filter (fun b -> subtype (cons_branch_type b) t)
      |> List.map (fun (t1,t2) ->
        combine (aux e1 t1) (aux e2 t2)
      ) |> List.flatten
    | Constructor (RecUpd label, [e;e']) ->
      let t = cap t (record_any_with label) in
      record_dnf t
      |> List.map (fun b -> record_branch_type b)
      |> List.filter (fun ti -> subtype ti t)
      |> List.map (fun ti ->
        let field_type = get_field ti label in
        let ti = remove_field_info ti label in
        combine (aux e ti) (aux e' field_type)
      ) |> List.flatten
    | Constructor (RecDel label, [e]) ->
      let t = cap t (record_any_without label) in
      record_dnf t
      |> List.map (fun b -> record_branch_type b)
      |> List.filter (fun ti -> subtype ti t)
      |> List.map (fun ti ->
        aux e (remove_field_info ti label)
      ) |> List.flatten
    | Constructor (Tag tag, [e]) -> aux e (destruct_tag tag t)
    | Constructor (Atom a, []) ->
      if subtype (mk_atom a) t then [REnv.empty] else []
    | Constructor _ -> assert false
    | TypeCoerce (_, s, _) when subtype (GTy.lb s) t -> [REnv.empty]
    | Abstract s when subtype (GTy.lb s) t -> [REnv.empty]
    | Const c when subtype (typeof_const c) t -> [REnv.empty]
    | ControlFlow _ when subtype unit_typ t -> [REnv.empty]
    | Abstract _ | Const _ | TypeCoerce _ | ControlFlow _ -> []
    | Projection (p, e) -> aux e (Checker.domain_of_proj p t)
    | TypeCast (e, _) -> aux e t
    | App ((_, Var v), e) when Env.mem v env ->
      let alpha = TVar.mk None in
      let (mono, ty) = Env.find v env |> TyScheme.get_fresh in
      let mono = TVarSet.union mono (vars t) in
      begin match dnf (GTy.lb ty) with
      | [] -> []
      | [arrows] ->
        let t1 = branch_type arrows in
        let res = tallying mono [ (t1, mk_arrow (TVar.typ alpha) t) ] in
        res |> List.map (fun sol ->
          let targ = Subst.find sol alpha |> top_instance mono in
          if is_undesirable mono targ then [] else aux e targ
        )
        |> List.flatten
      | _ -> []
      end
    | App _ -> []
    | Ite (e, s, e1, e2) ->
      let r1 = combine (aux e s) (aux e1 t) in
      let r2 = combine (aux e (neg s)) (aux e2 t) in
      r1@r2
    | Let (_, _, _, _) -> []
  in
  aux e t

let refine env e t =
  let base_renv = REnv.empty in
  let renvs = sufficient_refinements env e (neg t) in
  let rec aux renv renvs =
    let renvs = renvs |> List.map (fun renv' ->
      renv' |> REnv.filter (fun v ty ->
        let _, ty' = Env.find v env |> TyScheme.get in
        let ty'' = REnv.find' v renv in
        subtype (cap (GTy.lb ty') ty'') ty |> not
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
    | Abstract _ | Const _ | Var _ -> ()
    | Constructor (_, es) -> es |> List.iter (aux env)
    | Projection (_, e) | TypeCast (e, _) | TypeCoerce (e, _, _) -> aux env e
    | Lambda (d, v, e) -> aux_lambda env (d,v,e)
    | LambdaRec lst -> lst |> List.iter (aux_lambda env)
    | Ite (e, tau, e1, e2) | ControlFlow (_, e, tau, e1, e2) ->
      add_refinement env e tau ; add_refinement env e (neg tau) ;
      aux env e1 ; aux env e2
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

module Partitioner = struct
  type t = REnv.t list

  let isolate_tuple_comp (n,lst) =
    lst |>
    List.filter (function [] -> false | _ -> true) |>
    List.map (fun atom -> n, [atom])
  let isolate_tuple_conjuncts t =
    let (comps, _) = tuple_decompose t in
    let comps = comps |> List.map (fun cp -> isolate_tuple_comp cp) |> List.flatten in
    let comps = comps |> List.map (fun cp -> tuple_recompose ([cp], false)) in
    comps
  let isolate_record_conjuncts t =
    record_dnf t |>
    List.filter (function [], _ -> false | _, _ -> true) |>
    List.map (fun atom -> record_of_dnf [atom])
  let isolate_conjuncts t =
    (* Necessary because of pattern matching encoding for uncurrified functions *)
    t::(isolate_tuple_conjuncts t)@(isolate_record_conjuncts t)

  let from_renvset rs = REnvSet.elements rs
  let filter_compatible lst v ty =
    lst |> List.filter (fun renv ->
      (REnv.mem v renv |> not) ||
      (disjoint ty (REnv.find v renv) |> not)
    )
  let partition_for t v extra =
    let tys = t |> List.filter_map (fun renv ->
      if REnv.mem v renv then Some (REnv.find v renv) else None
    ) |> List.map isolate_conjuncts |> List.flatten in
    extra@tys |> Types.Additions.partition
    (* |> (fun tys -> Format.printf "Partition for %a: %a@." Parsing.Variable.Variable.pp v
      (Utils.pp_list pp_typ) tys ; tys) *)
end
