open Ast
open Env
open Types
open Types.Base
open Types.Tvar

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

let refine env renv e t =
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

let partition ts =
  let cap_if_nonempty t t' =
    let s = cap t t' in
    if is_empty s then t else s
  in
  let rec aux t =
    if is_empty t then []
    else
      let s = List.fold_left cap_if_nonempty t ts in
      s::(aux (diff t s))
  in
  aux any

let typeof env (_,e) =
  match e with
  | Var v when Env.mem v env -> Env.find v env
  | _ -> TyScheme.mk_mono any

let rec infer env renv (id,e) =
  let e, renvs = match e with
  | Abstract t -> Abstract t, []
  | Const c -> Const c, []
  | Var v -> Var v, []
  | Atom a -> Atom a, []
  | Tag (tag, e) ->
    let e, renvs = infer env renv e in
    Tag (tag, e), renvs
  | Lambda (ts, v, e) ->
    let t = disj ts |> TyScheme.mk_mono in
    let env = Env.add v t env in
    let e, renvs = infer env renv e in
    Lambda (ts, v, e), renvs
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
    let part = partition (part@tys) in
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
