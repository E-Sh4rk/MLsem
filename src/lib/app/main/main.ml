open Mlsem_common
open IO
open Mlsem_types.Builder
open Mlsem_types
open Mlsem_system.Ast
module MVariable = Mlsem_lang.MVariable

type def = Variable.t * PAst.expr * Ty.t option

exception IncompatibleType of Variable.t * TyScheme.t
exception UnresolvedType of Variable.t * TyScheme.t
exception Untypeable of Variable.t option * Mlsem_system.Checker.error

module NameMap = PAst.NameMap

let sigs_of_ty mono ty =
  let rec aux ty =
    match Arrow.dnf ty with
    | [arrs] ->
      let arrs = arrs |> List.concat_map
        (fun (a,b) -> aux b |> List.map (fun b -> Arrow.mk a b))
      in
      if Ty.equiv ty (Ty.conj arrs)
      then arrs else [ty]
    | _ -> [ty]
  in
  if TVOp.vars ty
    |> TVarSet.filter (fun tv -> TVar.has_kind KNoInfer tv |> not)
    |> TVarSet.is_empty then
    let sigs = aux ty in
    Some (sigs, GTy.mk ty |> TyScheme.mk_poly_except mono |> TyScheme.norm_and_simpl)
  else None
let infer var env e =
  let annot =
    (* Format.printf "@.@.%a@.@." Mlsem_system.Ast.pp e ; *)
    let r =
      if !Config.type_narrowing
      then Mlsem_system.Refinement.refinement_envs env e
      else REnvSet.empty
    in
    (* REnvSet.elements r |> List.iter (fun renv -> Format.printf "Renv: %a@." REnv.pp renv) ; *)
    try Mlsem_system.Reconstruction.infer env r e with
    | Mlsem_system.Checker.Untypeable err ->
      (* Format.printf "@.@.%a@.@." Mlsem_system.Ast.pp e ; *)
      raise (Untypeable (var, err))
  in
  let ty = Mlsem_system.Checker.typeof_def env annot e |> TyScheme.norm_and_simpl in
  let (tvs, ty) = TyScheme.get ty in
  let ty = TyScheme.mk tvs (GTy.ub ty |> GTy.mk) in
  let msg = Mlsem_system.Analyzer.analyze e annot in
  ty, msg
let retrieve_time time =
  let time' = Unix.gettimeofday () in
  (time' -. time) *. 1000.
let check_resolved var env typ =
  if TVarSet.diff (TyScheme.fv typ) (Env.tvars env) |> TVarSet.is_empty |> not
  then raise (UnresolvedType (var,typ))

let type_check_with_sigs env (var,e,sigs,aty) =
  if sigs = [] then
    (* Dyn type *)
    (var, aty), []
  else
    let e = Transform.expr_to_ast e in
    let c = if !Config.allow_implicit_downcast then CheckStatic else Check in
    let es = List.map (fun s -> coerce c (GTy.mk s) e) sigs in
    let typs, msg = List.map (infer (Some var) env) es |> List.split in
    let msg = (List.concat msg)@(Mlsem_system.Analyzer.get_unreachable e) in
    let tscap t1 t2 =
      let (tvs1, t1), (tvs2, t2) = TyScheme.get t1, TyScheme.get t2 in
      TyScheme.mk (TVarSet.union tvs1 tvs2) (GTy.cap t1 t2)
    in
    let typ = List.fold_left tscap (TyScheme.mk_mono GTy.any) typs |> TyScheme.norm_and_simpl in
    if TyScheme.leq typ aty |> not then raise (IncompatibleType (var,typ)) ;
    check_resolved var env typ ;
    (var,typ),msg

let type_check_recs pos env lst =
  let e =
    Eid.unique_with_pos pos,
    PAst.LambdaRec (List.map (fun (v,e) -> (v,None,e)) lst) in
  let e = Transform.expr_to_ast e in
  let ty, msg = infer None env e in
  let msg = msg@(Mlsem_system.Analyzer.get_unreachable e) in
  let tvs, ty = ty |> TyScheme.get in
  let n = List.length lst in
  List.mapi (fun i (var,_) ->
    let ty = TyScheme.mk tvs (GTy.map (Tuple.proj n i) ty) |> TyScheme.bot_instance in
    check_resolved var env ty ;
    (var, ty)
  ) lst, msg

type message = Mlsem_system.Analyzer.severity * Position.t * string * string option
type 'a treat_result =
| TSuccess of (Variable.t * string) list * message list * float
| TDone
| TFailure of Variable.t option * Position.t * string * string option * float

exception AlreadyDefined of Variable.t

let check_not_defined varm str =
  if NameMap.mem str varm then raise (AlreadyDefined (NameMap.find str varm))

let sigs_of_def benv varm senv env (kind,str) =
  let kind, benv = match kind with
  | PAst.Immut -> MVariable.Immut, benv | PAst.Mut -> MVariable.Mut, benv
  | PAst.AnnotMut ty ->
    let ty, benv = type_expr_to_typ benv ty in
    MVariable.AnnotMut ty, benv
  in
  match NameMap.find_opt str varm with
  | None ->
    let var = MVariable.create kind (Some str) in
    var, None, benv
  | Some v ->
    begin match VarMap.find_opt v senv with
    | None -> raise (AlreadyDefined v)
    | Some sigs ->
      if MVariable.kind_leq (MVariable.kind v) kind |> not then raise (AlreadyDefined v) ;
      v, Some (sigs, Env.find v env), benv
    end

let dummy = Variable.create (Some "_")
let treat (benv,varm,senv,env) (annot, elem) =
  let pos = Position.position annot in
  let time = Unix.gettimeofday () in
  let v = ref dummy in
  try  
    match elem with
    | PAst.Definitions lst ->
      let varm, benv = ref varm, ref benv in
      let lst = lst |> List.map (fun ((kind, name), e) ->
        let var, sigs, benv' = sigs_of_def !benv !varm senv env (kind, name) in
        benv := benv' ; Variable.attach_location var (Position.position annot) ;
        varm := NameMap.add name var !varm ;
        (var, e, sigs)
      )
      in
      let varm = !varm in
      let sigs, recs = List.partition_map (fun (var, e, sigs) ->
        v := var ;
        let e, benv' = PAst.parser_expr_to_expr !benv varm e in
        benv := benv' ;
        match sigs with
        | None -> Either.Right (var, e)
        | Some (sigs,aty) -> Either.Left (var, e, sigs, aty)
        ) lst in
      let tys1, msg1 = type_check_recs pos env recs in
      let env = List.fold_left (fun env (v,ty) ->
        try MVariable.add_to_env v ty env
        with Invalid_argument _ -> raise (IncompatibleType (v,ty))
      ) env tys1 in
      let tys2, msg2 = List.map (type_check_with_sigs env) sigs |> List.split in
      let msg = msg1@(List.concat msg2) |> List.map (fun r ->
        (r.Mlsem_system.Analyzer.severity, Eid.loc r.eid, r.title, r.descr)
      ) in
      let senv = List.fold_left (fun senv (v,_) -> VarMap.remove v senv) senv tys2 in
      let tys = tys1@tys2 |> List.map (fun (v, ty) -> v, Format.asprintf "%a" TyScheme.pp_short ty) in
      (!benv,varm,senv,env), TSuccess (tys,msg,retrieve_time time)
    | PAst.SigDef (name, mut, tyo) ->
      check_not_defined varm name ;
      begin match tyo with
      | None ->
        let kind = if mut then MVariable.Mut else MVariable.Immut in
        let v = MVariable.create kind (Some name) in
        Variable.attach_location v (Position.position annot) ;
        let varm = NameMap.add name v varm in
        let senv = VarMap.add v [] senv in
        let env = MVariable.add_to_env v (TyScheme.mk_mono GTy.dyn) env in
        (benv,varm,senv,env), TDone
      | Some ty ->
        let ty, benv = type_expr_to_typ benv ty in
        let kind = if mut then MVariable.AnnotMut ty else MVariable.Immut in
        let v = MVariable.create kind (Some name) in
        Variable.attach_location v (Position.position annot) ;
        begin match sigs_of_ty (Env.tvars env) ty with
        | None -> (benv,varm,senv,env),
          TFailure (Some v, pos, "Invalid signature annotation.", None, 0.0)
        | Some (sigs, ty) ->
          try
            let varm = NameMap.add name v varm in
            let senv = VarMap.add v sigs senv in
            let env = MVariable.add_to_env v ty env in
            (benv,varm,senv,env), TDone
          with Invalid_argument str -> (benv,varm,senv,env),
            TFailure (Some v, pos, str, None, 0.0)
        end
      end
    | PAst.Command (str, c) ->
      begin match str, c with
      | "value_restriction", Bool b -> Config.value_restriction := b
      | "type_narrowing", Bool b -> Config.type_narrowing := b
      | "allow_implicit_downcast", Bool b -> Config.allow_implicit_downcast := b
      | "infer_overload", Bool b -> Config.infer_overload := b
      | "no_empty_param", Bool b -> Config.no_empty_param := b
      | "no_abstract_inter", Bool b -> Config.no_abstract_inter := b
      | _ -> failwith ("Invalid command "^str)
      end ;
      (benv,varm,senv,env), TDone
    | PAst.Types lst ->
      let benv = define_aliases benv lst in
      (benv,varm,senv,env), TDone
    | PAst.AbsType (name, n) ->
      let benv = define_abstract benv name n in
      (benv,varm,senv,env), TDone
  with
  | PAst.SymbolError msg -> (benv,varm,senv,env), TFailure (Some !v, pos, msg, None, 0.0)
  | TypeDefinitionError msg -> (benv,varm,senv,env), TFailure (None, pos, msg, None, 0.0)
  | AlreadyDefined v ->
    (benv,varm,senv,env), TFailure (Some v, pos, "Symbol already defined.", None, 0.0)
  | Untypeable (v', err) ->
    let v = match v' with None -> !v | Some v -> v in
    let pos =
      if err.eid = Eid.dummy
      then Variable.get_location v
      else Eid.loc err.eid
    in
    (benv,varm,senv,env), TFailure (Some v, pos, err.title, err.descr, retrieve_time time)
  | IncompatibleType (var,_) ->
    (benv,varm,senv,env), TFailure (Some var, Variable.get_location var,
      "the type inferred is not a subtype of the type specified", None,
      retrieve_time time)
  | UnresolvedType (var,ty) ->
    (benv,varm,senv,env), TFailure (Some var, Variable.get_location var,
      "the type inferred is not fully resolved",
      Some (Format.asprintf "type inferred: %a" TyScheme.pp_short ty),
      retrieve_time time)

let treat (benv,varm,senv,env,penv) e =
  (* TODO: optimize aliases by tracking dependencies *)
  let ((benv,varm,senv,env),r), penv = PEnv.sequential_handler penv (treat (benv,varm,senv,env)) e in
  (benv,varm,senv,env,penv),r

let treat_sig envs (annot,elem) =
  let open PAst in
  match elem with
  | Types _ | AbsType _ | SigDef _ -> treat envs (annot,elem)
  | Command _ | Definitions _ -> envs, TDone
let treat_def envs (annot,elem) =
  let open PAst in
  match elem with
  | Types _ | AbsType _ | SigDef _ -> envs, TDone
  | Command _ | Definitions _ -> treat envs (annot,elem)
let treat_all_sigs envs elts =
  let rec aux envs elts =
    match elts with
    | [] -> envs, TDone
    | e::elts ->
      begin match treat_sig envs e with
      | (envs, TDone) -> aux envs elts
      | (envs, (TFailure _ as f)) -> envs, f
      | (_, TSuccess _) -> assert false
      end
  in
  aux envs elts

let builtin_functions = []
let initial_varm =
  builtin_functions |> List.fold_left (fun varm (name, _) ->
    let var = MVariable.create Immut (Some name) in
    NameMap.add name var varm
  ) PAst.empty_name_var_map
let initial_env =
  builtin_functions |> List.fold_left (fun env (name, t) ->
    let var = NameMap.find name initial_varm in
    Env.add var t env
  ) Env.empty

let initial_senv = VarMap.empty
let initial_benv = empty_benv
let initial_penv = PEnv.empty
let initial_envs = initial_benv, initial_varm, initial_senv, initial_env, initial_penv

type parsing_result =
| PSuccess of PAst.parser_program
| PFailure of Position.t * string

let parse f =
  try
    let p = match f with
      | `File fn -> parse_program_file fn
      | `String s -> parse_program_string s
    in
    PSuccess p
  with
  | PAst.LexicalError(pos, msg) -> PFailure (pos, msg)
  | PAst.SyntaxError (pos, msg) -> PFailure (pos, msg)
