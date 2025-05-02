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
  | Var _ -> assert false
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
    let _, t1 = Checker.typeof_proj p |> TyScheme.get_fresh in
    let cs, t2 = constr env renv e in
    let tv = TVar.mk None |> TVar.typ in
    (t1, mk_arrow t2 tv)::cs, tv
  | RecordUpdate _ ->
    (* TODO: we could be more precise with row polymorphism *)
    [], record_any
  | Let _ -> [], any
  | TypeConstr (e, _) -> constr env renv e
  | TypeCoerce (_, tys) -> [], conj tys

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
