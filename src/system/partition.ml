open Ast
open Env
open Types
open Types.Base
open Types.Tvar
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

let rec refine env (_,e) t =
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
      List.map2 (fun e t -> refine env e t) es ts
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

let typeof env (_,e) =
  match e with
  | Var v when Env.mem v env -> Env.find v env
  | _ -> TyScheme.mk_mono any

let refine_partitions env e =
  let parts = Hashtbl.create 100 in
  let get_parts v =
    match Hashtbl.find_opt parts v with Some lst -> lst | None -> []
  in
  let add_parts v tys =
    let lst = tys@(get_parts v) in
    Hashtbl.replace parts v lst
  in
  let refine env e t =
    refine env e (neg t) |> List.iter (fun renv ->
        REnv.bindings renv |> List.iter (fun (v,ty) -> add_parts v [neg ty])
      ) ;
  in
  let rec aux_lambda env (d,v,e) =
    let t = TyScheme.mk_mono d in
    d,v,aux (Env.add v t env) e
  and aux env (id,e) =
    let e = match e with
    | Abstract t -> Abstract t
    | Const c -> Const c
    | Var v -> Var v
    | Atom a -> Atom a
    | Tag (tag, e) -> Tag (tag, aux env e)
    | Lambda (d, v, e) ->
      let d,v,e = aux_lambda env (d,v,e) in
      Lambda (d,v,e)
    | LambdaRec lst ->
      LambdaRec (lst |> List.map (aux_lambda env))
    | Ite (e, tau, e1, e2) ->
      refine env e tau ; refine env e (neg tau) ;
      Ite (aux env e, tau, aux env e1, aux env e2)
    | App (e1, e2) -> App (aux env e1, aux env e2)
    | Tuple es -> Tuple (List.map (aux env) es)
    | Cons (e1, e2) -> Cons (aux env e1, aux env e2)
    | Projection (p, e) -> Projection (p, aux env e)
    | RecordUpdate (e, lbl, None) -> RecordUpdate (aux env e, lbl, None)
    | RecordUpdate (e, lbl, Some e') -> RecordUpdate (aux env e, lbl, Some (aux env e'))
    | Let (tys, v, e1, e2) ->
      let e1, e2 = aux env e1, aux (Env.add v (typeof env e1) env) e2 in
      let tys = (get_parts v)@tys |> partition in
      tys |> List.iter (refine env e1) ;
      Let (tys, v, e1, e2)
    | TypeConstr (e, tys) -> TypeConstr (aux env e, tys)
    | TypeCoerce (e, tys) -> TypeCoerce (aux env e, tys)
    in
    (id, e)
  in
  aux env e
