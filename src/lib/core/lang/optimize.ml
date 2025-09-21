open MAst
open Mlsem_common

type env = { captured:VarSet.t ; map:Variable.t VarMap.t }

let optimize_cf e =
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
    let map, e = match e with
    | Exc | Void | Value _ -> env, e
    | Voidify e -> let (map, (_,e)) = aux env e in map, e
    | Var v when VarMap.mem v env.map -> env, Var (VarMap.find v env.map)
    | Var v -> env, Var v
    | Constructor (c, es) ->
      let envs, es = List.map (aux env) es |> List.split in
      merge_envs' env envs, Constructor (c, es)
    | Lambda (tys, ty, v, e) ->
      let v, e =
        if MVariable.is_mutable v then
          let v' = MVariable.create_lambda MVariable.Immut (Variable.get_name v) in
          let _, e = aux { env with map=VarMap.singleton v v' } e in
          let e = Eid.unique (), Let ([], v, (Eid.unique (), Var v'), e) in
          v', e
        else
          v, aux { env with map=VarMap.empty } e |> snd
      in
      let fv = fv e in
      let env = { map=List.fold_right VarMap.remove (VarSet.elements fv) env.map ;
                  captured=VarSet.union env.captured fv } in
      env, Lambda (tys, ty, v, e)
    | LambdaRec lst ->
      let envs, es = List.map (fun (_,_,e) -> aux env e) lst |> List.split in
      merge_envs' env envs, LambdaRec (List.map2 (fun e (d,v,_) -> d,v,e) es lst)
    | Ite (e, ty, e1, e2) ->
      let env, e = aux env e in
      let (env1, e1), (env2, e2) = aux env e1, aux env e2 in
      merge_envs env1 env2, Ite (e, ty, e1, e2)
    | App (e1, e2) ->
      let (env1, e1), (env2, e2) = aux env e1, aux env e2 in
      merge_envs env1 env2, App (e1, e2)
    | Projection (p, e) ->
      let env, e = aux env e in
      env, Projection (p, e)
    | Declare (v, e) ->
      let env, e = aux env e in
      env, Declare (v, e)
    | Let (tys, v, e1, e2) when MVariable.is_mutable v ->
      let v' = MVariable.create_lambda MVariable.Immut (Variable.get_name v) in
      let env, e1 = aux { env with map=VarMap.singleton v v' } e1 in
      let env, e2 = aux env e2 in
      let e2 = Eid.unique (), Let ([], v, (Eid.unique (), Var v'), e2) in
      env, Let (tys, v', e1, e2)
    | Let (tys, v, e1, e2) ->
      let env, e1 = aux env e1 in
      let env, e2 = aux env e2 in
      env, Let (tys, v, e1, e2)
    | _ -> failwith "TODO"
    in
    map, (id, e)
  in
  aux { captured=VarSet.empty ; map=VarMap.empty } e
