open MAst
open Mlsem_common

let written_vars e =
  let wv = ref VarSet.empty in
  let aux (_,e) = match e with
  | VarAssign (v,_) -> wv := VarSet.add v !wv
  | _ -> ()
  in
  iter aux e ; !wv

let read_vars e =
  let rv = ref VarSet.empty in
  let aux (_,e) = match e with
  | Var v -> rv := VarSet.add v !rv
  | _ -> ()
  in
  iter aux e ; !rv

type env = { captured:VarSet.t ; immut:Variable.t VarMap.t ; mut:Variable.t list VarMap.t }

let optimize_cf e =
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
      | Some v1, Some v2 when Variable.equals v1 v2 -> Some v1
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
  let has_immut env v = VarMap.mem v env.immut in
  let get_immut env v = VarMap.find v env.immut in
  let get_muts env v =
    match VarMap.find_opt v env.mut with None -> [] | Some mut -> mut
  in
  let add_mut env v v' =
    { env with mut = VarMap.add v (v'::(get_muts env v)) env.mut }|> norm
  in
  let has_mut env v = get_muts env v |> List.is_empty |> not in
  let get_mut env v = get_muts env v |> List.hd in
  let reset_env env =
    { captured=env.captured ; mut=VarMap.empty ; immut=VarMap.empty }
  in
  let rec aux env (id, e) =
    match e with
    | Hole _ -> failwith "Unsupported hole."
    | Exc | Void | Value _ -> env, hole, (id, e)
    | Voidify e ->
      (* It would be unsound to move an expr of type empty outside *)
      let env, e = aux' env e in
      env, hole, (id, Voidify e)
    | Var v when has_immut env v -> env, hole, (id, Var (get_immut env v))
    | Var v when has_mut env v -> env, hole, (id, Var (get_mut env v))
    | Var v -> env, hole, (id, Var v)
    | Constructor (c, es) ->
      let wv = List.map written_vars es |> List.fold_left VarSet.union VarSet.empty in
      let env = restrict_immut env wv in
      let envs, es = List.map (aux' env) es |> List.split in
      merge_envs' env envs, hole, (id, Constructor (c, es))
    | Lambda (tys, ty, v, e) ->
      let v, e =
        if MVariable.is_mutable v then
          let v' = MVariable.create_lambda MVariable.Immut (Variable.get_name v) in
          let _, e = aux' (add_immut (reset_env env) v v') e in
          let e = Eid.unique (), Let ([], v, (Eid.unique (), Var v'), e) in
          v', e
        else
          v, aux' (reset_env env) e |> snd
      in
      let env = add_captured env (written_vars e) in
      env, hole, (id, Lambda (tys, ty, v, e))
    | LambdaRec lst ->
      let envs, es = List.map (fun (_,_,e) -> aux' env e) lst |> List.split in
      merge_envs' env envs, hole, (id, LambdaRec (List.map2 (fun e (d,v,_) -> d,v,e) es lst))
    | Ite (e, ty, e1, e2) ->
      let env, ctx, e = aux env e in
      let (env1, e1), (env2, e2) = aux' env e1, aux' env e2 in
      merge_envs' env [env1 ; env2], ctx, (id, Ite (e, ty, e1, e2))
    | App (e1, e2) ->
      let (env1, e1), (env2, e2) = aux' env e1, aux' env e2 in
      merge_envs' env [env1 ; env2], hole, (id, App (e1, e2))
    | Projection (p, e) ->
      let env, ctx, e = aux env e in
      env, ctx, (id, Projection (p, e))
    | Declare (v, e) ->
      let env, ctx, e = aux env e in
      env, (id, Declare (v, ctx)), e
    | Let (tys, v, e1, e2) when MVariable.is_mutable v ->
      let v' = MVariable.create_lambda MVariable.Immut (Variable.get_name v) in
      let env, ctx1, e1 = aux env e1 in
      let ctx1 = fill ctx1 (Eid.unique (), Let (tys, v', e1, hole)) in
      let ctx1 = fill ctx1 (Eid.unique (), Let ([], v, (Eid.unique (), Var v'), hole)) in
      let env, ctx2, e2 = aux (add_immut env v v') e2 in
      env, fill ctx1 ctx2, e2
    | Let (tys, v, e1, e2) ->
      let env, ctx1, e1 = aux env e1 in
      let env, ctx2, e2 = aux env e2 in
      env, fill ctx1 (id, Let (tys, v, e1, ctx2)), e2
    | TypeCast (e, ty) ->
      let env, ctx, e = aux env e in
      env, ctx, (id, TypeCast (e, ty))
    | TypeCoerce (e, ty, c) ->
      let env, ctx, e = aux env e in
      env, ctx, (id, TypeCoerce (e, ty, c))
    | VarAssign (v, e) ->
      let env, ctx, e = aux env e in
      let vimmut = MVariable.create_let MVariable.Immut (Variable.get_name v) in
      let env = add_immut env v vimmut in
      let ctx = fill ctx (Eid.unique (), Let ([], vimmut, e, hole)) in
      let vmut = MVariable.create_let (MVariable.kind v) (Variable.get_name v) in
      let env = add_mut env v vmut in
      let ctx = fill ctx (Eid.unique (), Declare (vmut, hole)) in
      let vmuts = get_muts env v in
      let e = Eid.unique (), VarAssign (v, (Eid.unique (), Var vimmut)) in
      let add_assign e v =
        Eid.unique (), Seq (
          e,
          (Eid.unique (), VarAssign (v, (Eid.unique (), Var vimmut)))
        )
      in
      env, ctx, List.fold_left add_assign e vmuts
    | Seq (e1, e2) ->
      let env, ctx1, e1 = aux env e1 in
      let env, ctx2, e2 = aux env e2 in
      env, fill ctx1 (id, Seq (e1, ctx2)), e2
    | Try es ->
      let wv = List.map written_vars es |> List.fold_left VarSet.union VarSet.empty in
      let env = restrict_immut env wv in
      let envs, es = List.map (aux' env) es |> List.split in
      merge_envs' env envs, hole, (id, Try es)
  and aux' env e =
    let env', ctx, e = aux env e in
    merge_envs env env', fill ctx e
  in
  aux' { captured=VarSet.empty ; immut=VarMap.empty ; mut=VarMap.empty } e |> snd

(* === Cleaning === *)

let clean_unused_assigns e =
  let f rv (id, e) =
    match e with
    | VarAssign (v, e) when VarSet.mem v rv |> not -> id, Voidify e
    | e -> id, e
  in
  map (f (read_vars e)) e

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

let optimize_cf e =
  e |> optimize_cf |> clean_unused_assigns |> clean_unused_defs |> clean_nop
