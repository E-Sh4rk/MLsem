open Ast
open Env
open Types
open Types.Base
open Types.Tvar
open Types.Additions
open Utils

let rec constr env renv (_,e) =
  match e with
  | Abstract ty -> [], ty
  | Const c -> [], Checker.typeof_const c
  | Var v when REnv.mem v renv -> [], REnv.find v renv
  | Var v when Env.mem v env ->
    let (_,t) = Env.find v env |> TyScheme.get_fresh in
    [], t
  | Var _ -> [], any
  | Atom a -> [], mk_atom a
  | Tag (tag, e) ->
    let cs, t = constr env renv e in
    cs, mk_tag tag t
  | Lambda _ -> [], arrow_any
  | LambdaRec _ -> [], any
  | Ite (_,_,e1,e2) ->
    let cs1, t1 = constr env renv e1 in
    let cs2, t2 = constr env renv e2 in
    cs1@cs2, cup t1 t2
  | App ((_, Var v) as e1, e2) ->
    let cs1, t1 = constr env (REnv.rm v renv) e1 in
    let cs2, t2 = constr env renv e2 in
    let tv = TVar.mk None |> TVar.typ in
    (t1, mk_arrow t2 tv)::(cs1@cs2), tv
  | App (e1, e2) ->
    let cs1, t1 = constr env renv e1 in
    let cs2, t2 = constr env renv e2 in
    let tv = TVar.mk None |> TVar.typ in
    (t1, mk_arrow t2 tv)::(cs1@cs2), tv
  | Tuple es ->
    let css, ts = es |> List.map (constr env renv) |> List.split in
    List.flatten css, mk_tuple ts
  | Cons (e1, e2) ->
    let cs1, t1 = constr env renv e1 in
    let cs2, t2 = constr env renv e2 in
    cs1@cs2, mk_cons t1 t2
  | Projection (p, e) ->
    let cs, s = constr env renv e in
    let tv = TVar.mk None |> TVar.typ in
    let t = Checker.domain_of_proj p tv in
    (s, t)::cs, tv
  | RecordUpdate _ ->
    (* TODO: we could be more precise with row polymorphism *)
    [], record_any
  | Let _ -> [], any
  | TypeConstr (e, _) -> constr env renv e
  | TypeCoerce (_, ty) -> [], ty

(* let size e =
  Ast.fold (fun _ lst -> List.fold_left (+) 1 lst) e *)

let refine env renv e t =
  (* if size e <= 1 then *)
  let cs, s = constr env renv e in
  let cs = (s, t)::cs in
  let mono = TVarSet.union (Env.tvars env) (TVar.user_vars ()) in
  tallying mono cs |> List.filter_map (fun s ->
    let bindings = REnv.bindings renv
      |> List.map (fun (v,t) -> (v,Subst.apply s t)) in
    let clean = clean_subst' ~pos:any ~neg:empty mono (List.map snd bindings) in
    let bindings = bindings |> List.map (fun (v,t) -> (v,Subst.apply clean t)) in
    let renv' = REnv.construct bindings in
    if TVarSet.subset (REnv.tvars renv') mono
      && not (REnv.leq renv renv')
      && not (REnv.bindings renv' |> List.exists (fun (_,t) -> is_empty t))
    then Some renv'
    else None
  )
  (* else [] *)

let typeof env (_,e) =
  match e with
  | Var v when Env.mem v env -> Env.find v env
  | _ -> TyScheme.mk_mono any

let rec infer env renv (id,e) =
  let aux_lambda (d,v,e) =
    let t = TyScheme.mk_mono d in
    let env = Env.add v t env in
    let e, renvs = infer env renv e in
    (d,v,e), renvs
  in
  let e, renvs = match e with
  | Abstract t -> Abstract t, []
  | Const c -> Const c, []
  | Var v -> Var v, []
  | Atom a -> Atom a, []
  | Tag (tag, e) ->
    let e, renvs = infer env renv e in
    Tag (tag, e), renvs
  | Lambda (d, v, e) ->
    let (d,v,e), renvs = aux_lambda (d,v,e) in
    Lambda (d,v,e), renvs
  | LambdaRec lst ->
    let (lst, renvs) = lst |> List.map aux_lambda |> List.split in
    LambdaRec lst, List.concat renvs
  | Ite (e, tau, e1, e2) ->
    let renvs1' = refine env renv e (neg tau) in
    let renvs2' = refine env renv e tau in
    let (e,renvs) = infer env renv e in
    let (e1,renvs1) = infer env renv e1 in
    let (e2,renvs2) = infer env renv e2 in
    Ite (e, tau, e1, e2), [ renvs1';renvs2';renvs;renvs1;renvs2 ] |> List.concat
  | App (e1, e2) ->
    let e1, renvs1 = infer env renv e1 in
    let e2, renvs2 = infer env renv e2 in
    App (e1, e2), renvs1@renvs2
  | Tuple es ->
    let es, renvs = List.map (infer env renv) es |> List.split in
    Tuple es, List.concat renvs
  | Cons (e1, e2) ->
    let e1, renvs1 = infer env renv e1 in
    let e2, renvs2 = infer env renv e2 in
    Cons (e1, e2), renvs1@renvs2
  | Projection (p, e) ->
    let e, renvs = infer env renv e in
    Projection (p, e), renvs
  | RecordUpdate (e, lbl, None) ->
    let e, renvs = infer env renv e in
    RecordUpdate (e, lbl, None), renvs
  | RecordUpdate (e, lbl, Some e') ->
    let e, renvs = infer env renv e in
    let e', renvs' = infer env renv e' in
    RecordUpdate (e, lbl, Some e'), renvs@renvs'
  | Let (tys, v, e1, e2) ->
    let e1, renvs1 = infer env renv e1 in
    let env' = Env.add v (typeof env e1) env in
    let renv' = REnv.add v (TVar.mk None |> TVar.typ) renv in
    let e2, renvs2 = infer env' renv' e2 in
    let part = renvs2 |> List.map (REnv.find v) in
    let part = Types.Additions.partition (part@tys) in
    let renvs2 = List.map (REnv.rm v) renvs2 in
    let renvs = part |> List.map (fun s -> refine env renv e1 (neg s)) in
    Let (part, v, e1, e2), [renvs1 ; renvs2]@renvs |> List.flatten
  | TypeConstr (e, tys) ->
    let e, renvs = infer env renv e in
    TypeConstr (e, tys), renvs
  | TypeCoerce (e, tys) ->
    let e, renvs = infer env renv e in
    TypeCoerce (e, tys), renvs
  in
  (id,e), renvs

let infer env e =
  infer env REnv.empty e |> fst

(* =============================================== *)

let rec refine env (_,e) t =
  let rec is_undesirable_arrow s =
    subtype s arrow_any &&
    dnf s |> List.for_all
      (List.exists (fun (a, b) -> non_empty a && is_undesirable_arrow b))
  in
  let is_undesirable mono s =
    TVarSet.subset (vars s) mono |> not ||
    is_undesirable_arrow s
  in
  let combine rs1 rs2 =
    carthesian_prod rs1 rs2
    |> List.map (fun (r1, r2) -> REnv.cap r1 r2)
  in
  match e with
  | Lambda _ -> []
  | LambdaRec _ -> []
  | Var v -> [REnv.singleton v t]
  | TypeCoerce (_, s) when subtype s t -> [REnv.empty]
  | Abstract s when subtype s t -> [REnv.empty]
  | Const c when subtype (Checker.typeof_const c) t -> [REnv.empty]
  | Atom a when subtype (mk_atom a) t -> [REnv.empty]
  | Abstract _ | Const _ | Atom _ | TypeCoerce _ -> []
  | Tag (tag, e) -> refine env e (destruct_tag tag t)
  | Tuple es ->
    tuple_dnf (List.length es) t
    |> List.filter (fun b -> subtype (tuple_branch_type b) t)
    |> List.map (fun ts ->
      List.map2 (fun t e -> refine env e t) ts es
      |> carthesian_prod' |> List.map REnv.conj
    ) |> List.flatten
  | Cons (e1, e2) ->
    cons_dnf t
    |> List.filter (fun b -> subtype (cons_branch_type b) t)
    |> List.map (fun (t1,t2) ->
      combine (refine env e1 t1) (refine env e2 t2)
    ) |> List.flatten
  | Projection (p, e) -> refine env e (Checker.domain_of_proj p t)
  | RecordUpdate (e, label, None) ->
    let t = cap t (record_any_without label) in
    record_dnf t
    |> List.map (fun b -> record_branch_type b)
    |> List.filter (fun ti -> subtype ti t)
    |> List.map (fun ti -> 
      refine env e (remove_field_info ti label)
    ) |> List.flatten
  | RecordUpdate (e, label, Some e') ->
    let t = cap t (record_any_with label) in
    record_dnf t
    |> List.map (fun b -> record_branch_type b)
    |> List.filter (fun ti -> subtype ti t)
    |> List.map (fun ti ->
      let field_type = get_field ti label in
      let ti = remove_field_info ti label in
      combine (refine env e ti) (refine env e' field_type)
    ) |> List.flatten
  | TypeConstr (e, _) -> refine env e t
  | App ((_, Var v), e) when Env.mem v env ->
    let alpha = TVar.mk None in
    let (mono, ty) = Env.find v env |> TyScheme.get_fresh in
    let mono = TVarSet.union mono (vars t) in
    begin match dnf ty with
    | [] -> []
    | [arrows] ->
      let t1 = branch_type arrows in
      let res = tallying mono [ (t1, mk_arrow (TVar.typ alpha) t) ] in
      res |> List.map (fun sol ->
        let targ = Subst.find sol alpha |> top_instance mono in
        if is_undesirable mono targ then [] else refine env e targ
      )
      |> List.flatten
    | _ -> []
    end
  | App _ -> []
  | Ite (e, s, e1, e2) ->
    let r1 = combine (refine env e s) (refine env e1 t) in
    let r2 = combine (refine env e (neg s)) (refine env e2 t) in
    r1@r2
  | Let (_, _, _, _) -> []
