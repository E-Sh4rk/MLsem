open Mlsem_common
open Ast
open Mlsem_types
open TVOp
open Mlsem_utils

module Refinements = struct
  module EMap = Eid.Map
  type t = GREnv.t EMap.t * GREnv.t list
  let empty = EMap.empty, []
  let get (t,_) e =
    match EMap.find_opt e t with
    | None -> GREnv.empty
    | Some renv -> renv
  let get_anonymous (_,anon) = anon
  let add (t,anon) e renv =
    let prev = get (t,anon) e in
    let renv = GREnv.cap prev renv in
    let t = if GREnv.is_empty renv then EMap.remove e t else EMap.add e renv t in
    t, anon
  let add_anonymous (t,anon) renv = (t,renv::anon)
  let all (t,anon) = (EMap.bindings t |> List.map snd)@anon
  let map f (t,anon) =
    EMap.map f t, List.map f anon
end

let check_cast c t ty =
  match c with
  | Check -> GTy.leq t ty
  | CheckStatic -> Ty.leq (GTy.lb t) (GTy.lb ty)
  | NoCheck -> true

(* Mirrors the conditions of [Checker.typeof'], but without annotations:
   [None] means that [e] cannot be guaranteed to be typeable. *)
let rec typeof env (_,e) =
  let ( let* ) = Option.bind in
  let typeofl es =
    List.fold_right (fun e acc ->
      let* tys = acc in
      let* ty = typeof env e in
      Some (ty::tys)
    ) es (Some [])
  in
  match e with
  | Value gty -> Some gty
  | Var v -> Env.find_opt v env |> Option.map (fun ts -> TyScheme.get_fresh ts |> snd)
  | Constructor (c, es) ->
    let* tys = typeofl es in
    let doms = Ast.domains_of_construct c Ty.any in
    let check tys = doms |> List.exists (fun doms -> List.for_all2 Ty.leq tys doms) in
    GTy.opl check (Ast.construct c) tys
  | GConstructor (c, es) ->
    let* tys = typeofl es in
    if Ast.domains_of_gconstruct c |> List.exists (fun doms -> List.for_all2 GTy.leq tys doms)
    then Some (Ast.gconstruct c tys) else None
  | Projection (p, e) ->
    let* ty = typeof env e in
    let dom = Ast.domain_of_proj p Ty.any in
    GTy.op (fun ty -> Ty.leq ty dom) (Ast.proj p) ty
  | Ite (e, _, e1, e2) ->
    let* _ = typeof env e in
    let* t1 = typeof env e1 in
    let* t2 = typeof env e2 in
    Some (GTy.cup t1 t2)
  | Let (_, v, e1, e2) ->
    let* s = typeof_def env e1 in
    typeof (Env.add v s env) e2
  | TypeCast (e, ty, c) ->
    let* t = typeof env e in
    if check_cast c t ty then Some (GTy.cap t ty) else None
  | TypeCoerce (e, ty, c) ->
    let* t = typeof env e in
    if check_cast c t ty then Some ty else None
  | Alt (settings, es) ->
    begin match List.map2 (fun e b -> if b then typeof env e else None) es (settings.amask env)
                |> List.filter_map Fun.id with
    | [] -> None
    | tys -> Some (GTy.conj tys)
    end
  | Lambda _ | LambdaRec _ | App _ | Operation _ -> None
and typeof_def env e = typeof env e |> Option.map (Checker.generalize ~e env)

let combine rs1 rs2 =
  Utils.cartesian_prod rs1 rs2
  |> List.map (fun (r1, r2) -> REnv.cap r1 r2)
let combine' rss =
  Utils.cartesian_prod' rss |> List.map REnv.conj

(* An [REnv] is a conjunction of constraints, so it is unsatisfiable as soon as
   one of them is. Not to be confused with [REnv.is_empty], which tests for the
   absence of constraints, i.e. for the trivially *true* refinement. *)
let is_unsatisfiable renv =
  REnv.bindings renv |> List.exists (fun (_,ty) -> Ty.is_empty ty)

let sufficient_refinements ~safe env e t =
  (* The mode and the type to refine [e] against so that it passes the check [c]
     of a cast or coercion to [ty], if there is a check. [CheckStatic] only
     constrains the lower bound of the type of [e], while [Check] also constrains
     its upper bound. *)
  let check_target c ty =
    match c with
    | NoCheck -> None
    | CheckStatic -> Some (false, GTy.lb ty)
    | Check -> Some (true, GTy.lb ty)
  in
  let rec aux' ~safe env (_,e) t =
    let b, b' = if safe then GTy.ub, GTy.lb else GTy.lb, GTy.ub in
    let aux = aux' ~safe in
    let app t1 e2 =
      let alpha = TVar.mk KInfer None in
      let (mono, ty) = TyScheme.get_fresh t1 in
      let mono = MVarSet.union mono (vars t) in
      begin match Arrow.dnf (b ty) with
      | [] -> []
      | [arrows] when Ty.leq (b ty) (Arrow.of_dnf [arrows]) ->
        let t1 = Arrow.of_dnf [arrows] in
        let res = tally mono [ (t1, Arrow.mk (TVar.typ alpha) t) ] in
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
      let doms = Ast.domains_of_construct c Ty.any in
      Ast.domains_of_construct c t
      |> List.filter (fun ts -> doms |> List.exists (List.for_all2 Ty.leq ts))
      |> List.concat_map
        (fun ts -> List.map2 (fun e t -> aux env e t) es ts |> combine')
    | GConstructor _ -> []
    | Value s when Ty.leq (b s) t -> [REnv.empty]
    | Value _ -> []
    | TypeCoerce (e, s, c) when Ty.leq (b s) t ->
      begin match check_target c s with
      | None -> aux env e Ty.any
      | Some (safe, s) -> aux' ~safe env e s
      end
    | TypeCoerce _ -> []
    | TypeCast (e, s, c) ->
      let renvs = aux env e (Ty.cup t (Ty.neg (b s))) in
      begin match check_target c s with
      | None -> renvs
      | Some (safe, s) -> combine renvs (aux' ~safe env e s)
      end
    | Projection (p, e) ->
      aux env e (Ty.cap (Ast.domain_of_proj p t) (Ast.domain_of_proj p Ty.any))
    | App (e1, e2) ->
      begin match typeof_def env e1 with
      | None -> []
      | Some t1 -> app t1 e2
      end
    | Operation (o, e) -> app (Ast.fun_of_operation env o) e
    | Ite (e, s, e1, e2) ->
      let r1 = combine (aux env e (b' s)) (aux env e1 t) in
      let r2 = combine (aux env e (GTy.neg s |> b')) (aux env e2 t) in
      r1@r2
    | Alt (settings,es) ->
      List.map2 (fun e b -> if b then aux env e t else []) es (settings.amask env) |> List.concat
    | Let (_, v, e1, e2) ->
      (* If the type of [e1] is unknown, [v] is left out of the environment:
         its occurrences in [e2] can still be refined, as [e1] is refined
         accordingly below, but nothing relies on its type. *)
      let env' = match typeof_def env e1 with
      | None -> Env.rm v env
      | Some s -> Env.add v s env
      in
      aux env' e2 t
      |> List.concat_map (fun renv ->
          let renv, t = REnv.rm v renv, REnv.find' v renv in
          let renvs = aux env e1 t in
          List.map (REnv.cap renv) renvs
        )
    in
    renvs |> List.filter (fun renv -> is_unsatisfiable renv |> not)
  in
  aux' ~safe env e t

let refine ~safe env e t =
  let unsat = Ast.fv e |> VarSet.elements
  |> List.map (fun v -> v, Ty.empty) |> REnv.construct in
  let base_renv = REnv.empty in
  let nt = GTy.neg t |> if safe then GTy.lb else GTy.ub in
  let renvs = sufficient_refinements ~safe env e nt in
  let rec aux renv renvs =
    let renvs = renvs |> List.map (fun renv' ->
      renv' |> REnv.filter (fun v ty ->
        let ty' =
          match Env.find_opt v env with
          | None -> Ty.any
          | Some ty -> ty |> TyScheme.get_fresh |> snd
            |> if safe then GTy.ub else GTy.lb
        in
        let ty'' = REnv.find' v renv in
        Ty.leq (Ty.cap ty' ty'') ty |> not
      )
    )
    in
    let renv' = REnv.cap renv (List.map (REnv.neg_approx ~unsat) renvs |> REnv.conj) in
    if REnv.leq renv renv' then renv else aux renv' renvs
  in
  aux base_renv renvs

let refine env e t =
  let fvs = Ast.fv e in
  let non_gradual = GTy.non_gradual t &&
    Env.bindings env |> List.for_all (fun (v, ts) ->
      VarSet.mem v fvs |> not || TyScheme.non_gradual ts
  ) in
  if non_gradual
  then
    refine ~safe:true env e t |> REnv.bindings
    |> List.map (fun (v, ty) -> v, GTy.mk ty) |> GREnv.construct
  else
    let res_ub = refine ~safe:true env e t in
    let res_lb = refine ~safe:false env e t in
    let dom = VarSet.union
      (REnv.domain res_lb |> VarSet.of_list)
      (REnv.domain res_ub |> VarSet.of_list) in
    dom |> VarSet.elements |> List.map (fun v ->
      let lb, ub = REnv.find' v res_lb, REnv.find' v res_ub in
      (v, GTy.mk_gradual lb ub)
      ) |> GREnv.construct

let refinements
  ?(extra_checks=[]) ?(refine_on_typecases=true) ?(refine_on_casts=false) env e =
  let extra = Hashtbl.create 10 in
  extra_checks |> List.iter (fun (eid, ty) -> Hashtbl.add extra eid ty) ;
  let res = ref Refinements.empty in
  let add_refinement eid env e t =
    res := Refinements.add !res eid (refine env e t)
  in
  let add_anonymous_refinement env e t =
    res := Refinements.add_anonymous !res (refine env e t)
  in
  let param_type ty = Option.value ty ~default:GTy.any |> TyScheme.mk_mono in
  let rec aux env (id,e) : unit =
    let extra = Hashtbl.find_all extra id in
    extra |> List.iter (fun ty -> add_anonymous_refinement env (id,e) ty) ;
    match e with
    | Value _ | Var _ -> ()
    | Constructor (_, es) | GConstructor (_, es) -> es |> List.iter (aux env)
    | Projection (_, e) | TypeCoerce (e, _, _) | Operation (_, e) -> aux env e
    | Lambda (d, v, e) -> aux (Env.add v (d |> param_type) env) e
    | LambdaRec lst ->
      let env = List.fold_left (fun env (ty,v,_) ->
        Env.add v (param_type ty) env
      ) env lst in
      lst |> List.map (fun (_,_,e) -> e) |> List.iter (aux env)
    | TypeCast (e, tau, _) ->
      if refine_on_casts then add_anonymous_refinement env e tau ;
      aux env e
    | Ite (e, tau, e1, e2) ->
      if refine_on_typecases then begin
        if fv e1 |> VarSet.is_empty |> not then add_refinement (fst e1) env e tau ;
        if fv e2 |> VarSet.is_empty |> not then add_refinement (fst e2) env e (GTy.neg tau)
      end ;
      aux env e ; aux env e1 ; aux env e2
    | App (e1, e2) -> aux env e1 ; aux env e2
    | Alt (_,es) -> List.iter (aux env) es
    | Let (_, v, e1, e2) ->
      let s = typeof_def env e1 |> Option.value ~default:(TyScheme.mk_mono GTy.any) in
      aux env e1 ; aux (Env.add v s env) e2 ;
      let res' =
        !res |> Refinements.map (fun renv ->
          match GREnv.find_opt v renv with
          | None -> renv
          | Some ty -> GREnv.cap renv (refine env e1 ty)
        )
      in
      res := res'
  in
  aux env e ; !res

let partition dom ts =
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
  aux dom

module Partitioner = struct
  type t = GREnv.t list

  let from_refinements rs = Refinements.all rs
  let filter_compatible lst v ty =
    lst |> List.filter (fun renv ->
      (GREnv.mem v renv |> not) ||
      (Ty.disjoint ty (GREnv.find v renv |> GTy.ub) |> not)
    )
  let decomposition_for t v initial =
    let tys = t |> List.filter_map (GREnv.find_opt v) |> List.map GTy.ub in
    let part_for_dom dom = tys |> partition dom in
    List.concat_map part_for_dom initial
    (* |> (fun tys -> Format.printf "Partition for %a: %a@." Variable.pp v
      (Utils.pp_list Ty.pp) tys ; tys) *)
end
