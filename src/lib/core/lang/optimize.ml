open MAst
open Mlsem_common
open Mlsem_utils
module SA = Mlsem_system.Ast

let eval_order_of_constructor c =
  match c with
  | SA.Tuple _ -> !Config.tuple_eval_order
  | SA.Cons -> !Config.cons_eval_order
  | SA.Rec _ -> !Config.record_eval_order
  | SA.Tag _ | SA.Enum _ -> Config.LeftToRight
  | SA.Meet _ | SA.Join _ | SA.Negate | SA.Ternary _ -> Config.UnknownOrder
  | SA.CCustom c ->
    begin match Hashtbl.find_opt Config.ccustom_eval_order c.cname with
    | None -> Config.UnknownOrder
    | Some o -> o
    end

let written_vars e =
  let wv = ref VarSet.empty in
  let aux (_,e) = match e with
  | VarAssign (v,_) -> wv := VarSet.add v !wv
  | _ -> ()
  in
  iter aux e ; VarSet.inter (fv e) !wv

let read_vars e =
  let rv = ref VarSet.empty in
  let aux (_,e) = match e with
  | Var v -> rv := VarSet.add v !rv
  | _ -> ()
  in
  iter aux e ; VarSet.inter (fv e) !rv

type env = { captured:VarSet.t ; immut:Variable.t VarMap.t ; mut:Variable.t list VarMap.t }

let optimize_dataflow e =
  let hole = Eid.dummy, Hole 0 in
  let fill e elt = fill_hole 0 elt e in
  let norm env =
    let captured = VarSet.elements env.captured in
    { immut = List.fold_right VarMap.remove captured env.immut ;
      mut = List.fold_right VarMap.remove captured env.mut ;
      captured = env.captured }
  in
  let add_captured env vs =
    { env with captured=VarSet.union env.captured vs } |> norm
  in
  let restrict_immut env vs =
    { env with immut = List.fold_right VarMap.remove (VarSet.elements vs) env.immut }
  in
  let merge_envs benv nenv =
    let immut = VarMap.merge (fun _ v1 v2 ->
      match v1, v2 with
      | None, None | Some _, None | None, Some _ -> None
      | Some v1, Some v2 when Variable.equal v1 v2 -> Some v1
      | Some _, Some _ -> None
    ) benv.immut nenv.immut in
    let mut = benv.mut in
    let captured = VarSet.union benv.captured nenv.captured in
    { captured ; immut ; mut } |> norm
  in
  let merge_envs' benv nenvs =
    List.fold_left merge_envs benv nenvs
  in
  let add_immut env v v' =
    { env with immut = VarMap.add v v' env.immut }|> norm
  in
  let has_immut env v = MVariable.is_mutable v |> not || VarMap.mem v env.immut in
  let get_immut env v = if MVariable.is_mutable v then VarMap.find v env.immut else v in
  let get_muts env v =
    match VarMap.find_opt v env.mut with None -> [] | Some mut -> mut
  in
  let add_mut env v v' =
    { env with mut = VarMap.add v (v'::(get_muts env v)) env.mut }|> norm
  in
  let get_preferred_mut env v = match get_muts env v with hd::_ -> hd | [] -> v in
  let reset_env env =
    { captured=env.captured ; mut=VarMap.empty ; immut=VarMap.empty }
  in
  let rec aux env (id, e) =
    let env, ctx, e = match e with
    | Hole _ -> failwith "Unsupported hole."
    | Exc | Void | Value _ -> env, hole, (id, e)
    | Voidify e ->
      (* It would be unsound to move an expr of type empty outside *)
      let env, e = aux' env e in
      env, hole, (id, Voidify e)
    | Var v when has_immut env v -> env, hole, (id, Var (get_immut env v))
    | Var v ->
      let v' = MVariable.refresh MVariable.Immut v in
      let env = add_immut env v v' in
      let ctx = Eid.unique (), Let ([], v', (Eid.unique (), Var (get_preferred_mut env v)), hole) in
      env, ctx, (id, Var v')
    | Constructor (c, es) ->
      let env, ctx, es = aux_order (eval_order_of_constructor c) env es in
      env, ctx, (id, Constructor (c, es))
    | Lambda (tys, ty, v, e) ->
      let e = aux' (reset_env env) e |> snd in
      let env = add_captured env (written_vars e) in
      env, hole, (id, Lambda (tys, ty, v, e))
    | LambdaRec lst ->
      let es = List.map (fun (_,_,e) -> e) lst in
      let wv = List.map written_vars es |> List.fold_left VarSet.union VarSet.empty in
      let env = restrict_immut env wv in
      let envs, es = List.map (aux' env) es |> List.split in
      merge_envs' env envs, hole,
      (id, LambdaRec (List.map2 (fun (d,v,_) e -> d,v,e) lst es))
    | Ite (e, ty, e1, e2) ->
      let env, ctx, e = aux env e in
      let (env1, e1), (env2, e2) = aux' env e1, aux' env e2 in
      merge_envs' env [env1 ; env2], ctx, (id, Ite (e, ty, e1, e2))
    | App (e1, e2) ->
      let env, ctx, es = aux_order !Config.app_eval_order env [e1;e2] in
      env, ctx, (id, App (List.nth es 0, List.nth es 1))
    | Operation (o, e) ->
      let env, ctx, e = aux env e in
      env, ctx, (id, Operation (o, e))
    | Projection (p, e) ->
      let env, ctx, e = aux env e in
      env, ctx, (id, Projection (p, e))
    | Declare (v, e) ->
      let env, ctx, e = aux env e in
      env, (id, Declare (v, ctx)), e
    | Let (tys, v, e1, e2) when MVariable.is_mutable v ->
      let v' = MVariable.refresh MVariable.Immut v in
      let env, ctx1, e1 = aux env e1 in
      let ctx1 = fill ctx1 (Eid.unique (), Let (tys, v', e1, hole)) in
      let ctx1 = fill ctx1 (Eid.unique (), Let ([], v, (Eid.unique (), Var v'), hole)) in
      let env, ctx2, e2 = aux (add_immut env v v') e2 in
      env, fill ctx1 ctx2, e2
    | Let (tys, v, e1, e2) ->
      let env, ctx1, e1 = aux env e1 in
      let env, ctx2, e2 = aux env e2 in
      env, fill ctx1 (id, Let (tys, v, e1, ctx2)), e2
    | TypeCast (e, ty, c) ->
      let env, ctx, e = aux env e in
      env, ctx, (id, TypeCast (e, ty, c))
    | TypeCoerce (e, ty, c) ->
      let env, ctx, e = aux env e in
      env, ctx, (id, TypeCoerce (e, ty, c))
    | VarAssign (v, e) ->
      let env, ctx, e = aux env e in
      let vimmut = MVariable.refresh MVariable.Immut v in
      let env = add_immut env v vimmut in
      let ctx = fill ctx (Eid.unique (), Let ([], vimmut, e, hole)) in
      let vmut = MVariable.refresh (MVariable.kind v) v in
      let env = add_mut env v vmut in
      let ctx = fill ctx (Eid.unique (), Declare (vmut, hole)) in
      let vmuts = get_muts env v in
      let e = Eid.refresh id, VarAssign (v, (Eid.unique (), Var vimmut)) in
      let add_assign e v =
        Eid.refresh id, Seq (
          e,
          (Eid.refresh id, VarAssign (v, (Eid.unique (), Var vimmut)))
        )
      in
      env, ctx, List.fold_left add_assign e vmuts
    | Loop e ->
      let env = restrict_immut env (written_vars e) in
      let env, e = aux' env e in
      env, hole, (id, Loop e)
    | Seq (e1, e2) ->
      let env, ctx1, e1 = aux env e1 in
      let env, ctx2, e2 = aux env e2 in
      env, fill ctx1 (id, Seq (e1, ctx2)), e2
    | Try (e1, e2) ->
      let env, ctx, es = aux_parallel env [e1;e2] in
      env, ctx, (id, Try (List.nth es 0, List.nth es 1))
    | Alt (e1, e2) ->
      let (env1, e1), (env2, e2) = aux' env e1, aux' env e2 in
      merge_envs' env [env1;env2], hole, (id, Alt (e1, e2))
    in
    env, ctx, e
  and aux_parallel env es =
    let envs, es = es
      |> List.map (fun e -> e, written_vars e)
      |> Utils.add_others |> List.map (fun ((e,_), es) ->
      let wv = List.map snd es |> List.fold_left VarSet.union VarSet.empty in
      let env = restrict_immut env wv in
      aux' env e
      ) |> List.split in
    merge_envs' env envs, hole, es
  and aux_sequence env es =
    let f (env,ctx,es) e =
      let env', ctx', e' = aux env e in
      env', fill ctx ctx', e'::es
    in
    let env, ctx, es = List.fold_left f (env, hole, []) es in
    env, ctx, List.rev es
  and aux_sequence_rev env es =
    let env, ctx, es = aux_sequence env (List.rev es) in
    env, ctx, List.rev es
  and aux_order order =
    match order with
    | Config.LeftToRight -> aux_sequence
    | Config.RightToLeft -> aux_sequence_rev
    | Config.UnknownOrder -> aux_parallel
  and aux' env e =
    let env', ctx, e = aux env e in
    merge_envs env env', fill ctx e
  in
  aux' { captured=VarSet.empty ; immut=VarMap.empty ; mut=VarMap.empty } e |> snd

(* === Cleaning === *)

let captured_vars e =
  let cv = ref VarSet.empty in
  let aux (_,e) = match e with
  | Lambda (_, _, _, e) -> cv := VarSet.union !cv (read_vars e) ; false
  | _ -> true
  in
  iter' aux e ; !cv

let rec clean_unused_assigns e =
  let rec aux
      cv (* Captured variables (may be read at any moment) *)
      rv (* Variables that MAY be read before being written *)
      (id,e) =
    match e with
    | Hole _ -> failwith "Unsupported hole."
    | Exc | Void | Value _ -> (id, e), rv
    | Voidify e ->
      (* rv is now considered captured, because e may exit (exception) before the end *)
      let cv, rv = VarSet.union cv rv, VarSet.empty in
      let e, rv = aux cv rv e in (id, Voidify e), rv
    | Var v -> (id, Var v), VarSet.add v rv
    | Constructor (c, es) ->
      (* Note: unlike Voidify, we consider that a diverging argument makes the
         constructor diverge (which is technically not true for the Ignore constructor) *)
      let es, rv = aux_order (eval_order_of_constructor c) cv rv es in
      (id, Constructor (c, es)), rv
    | Lambda (tys, ty, v, e) ->
      (id, Lambda (tys, ty, v, clean_unused_assigns e)), rv
    | LambdaRec lst ->
      let es = List.map (fun (_,_,e) -> e) lst in
      let rv = List.map read_vars es |> List.fold_left VarSet.union rv in
      let es, rvs = List.map (aux cv rv) es |> List.split in
      (id, LambdaRec (List.map2 (fun (ty,v,_) e -> ty, v, e) lst es)),
      rvs |> List.fold_left VarSet.union rv
    | Ite (e, ty, e1, e2) ->
      let (e1, rv1), (e2, rv2) = aux cv rv e1, aux cv rv e2 in
      let rv = VarSet.union rv1 rv2 in
      let e, rv = aux cv rv e in
      (id, Ite (e,ty,e1,e2)), rv
    | App (e1, e2) ->
      let es, rv = aux_order !Config.app_eval_order cv rv [e1;e2] in
      (id, App (List.nth es 0, List.nth es 1)), rv
    | Operation (o, e) -> let e, rv = aux cv rv e in (id, Operation (o, e)), rv
    | Projection (p, e) -> let e, rv = aux cv rv e in (id, Projection (p, e)), rv
    | Declare (v, e) -> let e, rv = aux cv rv e in (id, Declare (v, e)), rv
    | Let (tys, v, e1, e2) ->
      let e2, rv = aux cv rv e2 in
      let e1, rv = aux cv rv e1 in
      (id, Let (tys, v, e1, e2)), rv
    | TypeCast (e, ty, c) -> let e, rv = aux cv rv e in (id, TypeCast (e, ty, c)), rv
    | TypeCoerce (e, ty, c) -> let e, rv = aux cv rv e in (id, TypeCoerce (e, ty, c)), rv
    | VarAssign (v, e) when VarSet.mem v (VarSet.union cv rv) ->
      let rv = VarSet.remove v rv in
      let e, rv = aux cv rv e in (id, VarAssign (v, e)), rv
    | VarAssign (_, e) -> let e, rv = aux cv rv e in (Eid.unique (), Voidify e), rv
    | Loop e ->
      let rv = VarSet.union rv (read_vars e) in
      let e, rv = aux cv rv e in
      (id, Loop e), rv
    | Seq (e1, e2) ->
      let e2, rv = aux cv rv e2 in
      let e1, rv = aux cv rv e1 in
      (id, Seq (e1, e2)), rv
    | Try (e1, e2) ->
      let es, rv = aux_parallel cv rv [e1;e2] in
      (id, Try (List.nth es 0, List.nth es 1)), rv
    | Alt (e1, e2) ->
      let (e1, rv1), (e2, rv2) = aux cv rv e1, aux cv rv e2 in
      let rv = VarSet.union rv1 rv2 in
      (id, Alt (e1,e2)), rv
  and aux_parallel cv rv es =
    let es, rvs = es
      |> List.map (fun e -> e, read_vars e)
      |> Utils.add_others |> List.map (fun ((e,_), es) ->
      let rv = List.map snd es |> List.fold_left VarSet.union rv in
      aux cv rv e
      ) |> List.split in
    es, rvs |> List.fold_left VarSet.union rv
  and aux_sequence cv rv es =
    let f e (rv, es) =
      let e', rv' = aux cv rv e in
      rv', e'::es
    in
    let rv, es = List.fold_right f es (rv, []) in
    es, rv
  and aux_order order =
    match order with
    | Config.LeftToRight -> aux_sequence
    | Config.RightToLeft -> aux_sequence_rev
    | Config.UnknownOrder -> aux_parallel
  and aux_sequence_rev cv rv es =
    let es, rv = aux_sequence cv rv (List.rev es) in
    List.rev es, rv
  in
  aux
    (VarSet.union (captured_vars e) (fv e (* Global vars *)))
    VarSet.empty e |> fst

let clean_unused_defs e =
  let f (id,e) =
    match e with
    | Declare (v, e) when VarSet.mem v (fv e) |> not -> e
    | Let (_, v, e1, e2) when VarSet.mem v (fv e2) |> not -> id, Seq (e1, e2)
    | e -> id, e
  in
  map f e

let clean_nop e =
  let f (id, e) =
    match e with
    | Voidify e when VarSet.is_empty (fv e) -> id, Void
    | Seq (e1, e2) when VarSet.is_empty (fv e1) -> e2
    | e -> id, e
  in
  map f e

let optimize_dataflow e =
  e |> optimize_dataflow |> clean_unused_assigns |> clean_unused_defs |> clean_nop
