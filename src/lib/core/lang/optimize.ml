open MAst
open Mlsem_common

let clean_unused_defs e =
  let f (id,e) =
    match e with
    | Declare (v, e) when VarSet.mem v (fv e) |> not -> e
    | Let (_, v, (_, Var _), e2) when VarSet.mem v (fv e2) |> not -> e2
    | e -> (id,e)
  in
  map f e

(* TODO: turn assign into let mut of a new var if possible by scoping
   (start from the end) *)
let split_mut_defs e =
  let rec aux uv (* vars that may be used after that point *) (id, e) =
    match e with
   | Hole _ | Exc | Void | Value _ | Var _ -> (id, e)
   | Constructor (c, es) ->
    let uv = List.fold_left VarSet.union uv (List.map fv es) in
    let es = List.map (aux uv) es in
    id, Constructor (c, es)
   | _-> failwith "TODO"
  in
  aux (fv e (* global vars *)) e
  [@@ocaml.warning "-32"]

let written_vars e =
  let wv = ref VarSet.empty in
  let aux (_,e) = match e with
  | VarAssign (v,_) -> wv := VarSet.add v !wv
  | _ -> ()
  in
  iter aux e ; !wv

type env = { captured:VarSet.t ; map:Variable.t VarMap.t }

let optimize_cf e =
  let hole = Eid.dummy, Hole 0 in
  let fill e elt = fill_hole 0 elt e in
  let restrict_env ~capture env vs =
    { map = List.fold_right VarMap.remove (VarSet.elements vs) env.map ;
      captured = if capture then VarSet.union env.captured vs else env.captured }
  in
  let merge_envs env1 env2 =
    let map = VarMap.merge (fun _ v1 v2 ->
      match v1, v2 with
      | None, None | Some _, None | None, Some _ -> None
      | Some v1, Some v2 when Variable.equals v1 v2 -> Some v1
      | Some _, Some _ -> None
    ) env1.map env2.map in
    let captured = VarSet.union env1.captured env2.captured in
    { captured ; map }
  in
  let merge_envs' env envs =
    List.fold_left merge_envs env envs
  in
  let rec aux env (id, e) =
    match e with
    | Hole _ | Exc | Void | Value _ -> env, hole, (id, e)
    | Voidify e ->
      (* It would be unsound to move an expr of type empty outside *)
      let env, e = aux' env e in
      env, hole, (id, Voidify e)
    | Var v when VarMap.mem v env.map -> env, hole, (id, Var (VarMap.find v env.map))
    | Var v -> env, hole, (id, Var v)
    | Constructor (c, es) ->
      let wv = List.map written_vars es |> List.fold_left VarSet.union VarSet.empty in
      let env = restrict_env ~capture:false env wv in
      let envs, es = List.map (aux' env) es |> List.split in
      merge_envs' env envs, hole, (id, Constructor (c, es))
    | Lambda (tys, ty, v, e) ->
      let v, e =
        if MVariable.is_mutable v then
          let v' = MVariable.create_lambda MVariable.Immut (Variable.get_name v) in
          let _, e = aux' { env with map=VarMap.singleton v v' } e in
          let e = Eid.unique (), Let ([], v, (Eid.unique (), Var v'), e) in
          v', e
        else
          v, aux' { env with map=VarMap.empty } e |> snd
      in
      let env = restrict_env ~capture:true env (written_vars e) in
      env, hole, (id, Lambda (tys, ty, v, e))
    | LambdaRec lst ->
      let envs, es = List.map (fun (_,_,e) -> aux' env e) lst |> List.split in
      merge_envs' env envs, hole, (id, LambdaRec (List.map2 (fun e (d,v,_) -> d,v,e) es lst))
    | Ite (e, ty, e1, e2) ->
      let env, ctx, e = aux env e in
      let (env1, e1), (env2, e2) = aux' env e1, aux' env e2 in
      merge_envs env1 env2, ctx, (id, Ite (e, ty, e1, e2))
    | App (e1, e2) ->
      let (env1, e1), (env2, e2) = aux' env e1, aux' env e2 in
      merge_envs env1 env2, hole, (id, App (e1, e2))
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
      let env, ctx2, e2 = aux { env with map=VarMap.singleton v v' } e2 in
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
      let v' = MVariable.create_lambda MVariable.Immut (Variable.get_name v) in
      let env, ctx, e = aux env e in
      let env = { env with map=VarMap.singleton v v' } in
      let ctx = fill ctx (Eid.unique (), Let ([], v', e, hole)) in
      env, ctx, (id, VarAssign (v, (Eid.unique (), Var v')))
    | Seq (e1, e2) ->
      let env, ctx1, e1 = aux env e1 in
      let env, ctx2, e2 = aux env e2 in
      env, fill ctx1 (id, Seq (e1, ctx2)), e2
    | Try es ->
      let wv = List.map written_vars es |> List.fold_left VarSet.union VarSet.empty in
      let env = restrict_env ~capture:false env wv in
      let envs, es = List.map (aux' env) es |> List.split in
      merge_envs' env envs, hole, (id, Try es)
  and aux' env e =
    let env', ctx, e = aux env e in
    merge_envs env env', fill ctx e
  in
  aux' { captured=VarSet.empty ; map=VarMap.empty } e |> snd

let optimize_cf e =
  e |> optimize_cf |> clean_unused_defs
