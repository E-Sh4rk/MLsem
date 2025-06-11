open Parsing.IO
open System
open Types.Base
open Types.Additions
open Types.Tvar
open Types
open Parsing
open Parsing.Variable
open Env

type def = Variable.t * Ast.expr * typ option

exception IncompatibleType of Variable.t * TyScheme.t
exception UnresolvedType of Variable.t * TyScheme.t
exception Untypeable of Variable.t

let sigs_of_ty mono ty =
  let rec aux ty =
    match dnf ty with
    | [arrs] ->
      let arrs = arrs |> List.map (fun (a,b) ->
        aux b |> List.map (fun b -> mk_arrow a b)
      ) |> List.flatten
      in
      if equiv ty (conj arrs)
      then arrs else [ty]
    | _ -> [ty]
  in
  if ty |> vars_internal |> TVarSet.is_empty then
    let sigs = aux ty in
    Some (sigs, TyScheme.mk_poly_except mono ty)
  else None
let infer var env e =
  (* let e = Partition.infer env e in *)
  let annot =
    let r = if !Config.type_narrowing then Partition.refinement_envs env e else REnvSet.empty in
    match Reconstruction.infer env r e with
    | None ->
      (* Format.printf "@.@.%a@.@." System.Ast.pp e ; *)
      raise (Untypeable var)
    | Some annot-> annot
  in
  Checker.typeof_def env annot e |> TyScheme.simplify
let retrieve_time time =
  let time' = Unix.gettimeofday () in
  (time' -. time) *. 1000.
let check_resolved var env typ =
  if TVarSet.diff (TyScheme.fv typ) (Env.tvars env) |> TVarSet.is_empty |> not
  then raise (UnresolvedType (var,typ))

let type_check_with_sigs env (var,e,sigs,aty) =
  let e = System.Ast.from_parser_ast e in
  let es = List.map (fun s -> System.Ast.coerce s e) sigs in
  let typs = List.map (infer var env) es in
  let tscap t1 t2 =
    let (tvs1, t1), (tvs2, t2) = TyScheme.get t1, TyScheme.get t2 in
    TyScheme.mk (TVarSet.union tvs1 tvs2) (cap t1 t2)
  in
  let typ = List.fold_left tscap (TyScheme.mk_mono any) typs in
  if TyScheme.leq typ aty |> not then raise (IncompatibleType (var,typ)) ;
  check_resolved var env typ ;
  var,typ

let type_check_recs env lst =
  if lst = [] then []
  else
    let e =
      Parsing.Ast.unique_exprid (),
      Parsing.Ast.LambdaRec (List.map (fun (v,e) -> (v,None,e)) lst) in
    let e = System.Ast.from_parser_ast e in
    let (var,_) = List.hd lst in
    let typ = infer var env e in
    check_resolved var env typ ;
    let tvs, ty = TyScheme.get typ in
    let n = List.length lst in
    List.mapi (fun i (var,_) ->
      (var, TyScheme.mk tvs (pi n i ty) |> TyScheme.bot_instance)
    ) lst

type 'a treat_result =
| TSuccess of (Variable.t * TyScheme.t) list * float
| TDone
| TFailure of Variable.t option * (Position.t list) * string * float

exception AlreadyDefined of Variable.t

let check_not_defined varm str =
  if StrMap.mem str varm then raise (AlreadyDefined (StrMap.find str varm))

let sigs_of_def varm senv env str =
  match StrMap.find_opt str varm with
  | None ->
    let var = Variable.create_let (Some str) in
    var, None
  | Some v ->
    begin match VarMap.find_opt v senv with
    | None -> raise (AlreadyDefined v)
    | Some sigs -> v, Some (sigs, Env.find v env)
    end

let treat (tenv,varm,senv,env) (annot, elem) =
  let pos = [Position.position annot] in
  let time = Unix.gettimeofday () in
  try  
    match elem with
    | Ast.Definitions lst ->
      let varm = ref varm in
      let lst = lst |> List.map (fun (name, e) ->
        let var, sigs = sigs_of_def !varm senv env name in
        Variable.attach_location var (Position.position annot) ;
        varm := StrMap.add name var !varm ;
        (var, e, sigs)
      )
      in
      let varm = !varm in
      let sigs, recs = List.partition_map (fun (var, e, sigs) ->
        let e = Ast.parser_expr_to_expr tenv empty_vtenv varm e in
        match sigs with
        | None -> Either.Right (var, e)
        | Some (sigs,aty) -> Either.Left (var, e, sigs, aty)
        ) lst in
      let tys1 = type_check_recs env recs in
      let env = List.fold_left (fun env (v,ty) -> Env.add v ty env) env tys1 in
      let tys2 = List.map (type_check_with_sigs env) sigs in
      let senv = List.fold_left (fun senv (v,_) -> VarMap.remove v senv) senv tys2 in
      (tenv,varm,senv,env), TSuccess (tys1@tys2,retrieve_time time)
    | Ast.SigDef (name, ty) ->
      check_not_defined varm name ;
      let v = Variable.create_let (Some name) in
      Variable.attach_location v (Position.position annot) ;
      let (ty, _) = type_expr_to_typ tenv empty_vtenv ty in
      begin match sigs_of_ty (Env.tvars env) ty with
      | None -> (tenv,varm,senv,env),
        TFailure (Some v, pos, "Invalid signature annotation.", 0.0)
      | Some (sigs, ty) ->
        let varm = StrMap.add name v varm in
        let senv = VarMap.add v sigs senv in
        let env = Env.add v ty env in
        (tenv,varm,senv,env), TDone
      end
    | Ast.Command (str, c) ->
      begin match str, c with
      | "log", Int i -> Config.log_level := Z.to_int i
      | "value_restriction", Bool b -> Config.value_restriction := b
      | "type_narrowing", Bool b -> Config.type_narrowing := b
      | _ -> failwith ("Invalid command "^str)
      end ;
      (tenv,varm,senv,env), TDone
    | Ast.Types lst ->
      let tenv = define_types tenv empty_vtenv lst in
      (tenv,varm,senv,env), TDone
    | Ast.AbsType (name, vs) ->
      let tenv = define_abstract tenv name vs in
      (tenv,varm,senv,env), TDone
  with
  | Ast.SymbolError msg -> (tenv,varm,senv,env), TFailure (None, pos, msg, 0.0)
  | TypeDefinitionError msg -> (tenv,varm,senv,env), TFailure (None, pos, msg, 0.0)
  | AlreadyDefined v ->
    (tenv,varm,senv,env), TFailure (Some v, pos, "Symbol already defined.", 0.0)
  | Untypeable (v) ->
    (* TODO: retrieve pos of exprid *)
    (tenv,varm,senv,env), TFailure (Some v, Variable.get_locations v,
      "annotation reconstruction failed", retrieve_time time)
  | IncompatibleType (var,_) ->
    (tenv,varm,senv,env), TFailure (Some var, Variable.get_locations var,
      "the type inferred is not a subtype of the type specified",
      retrieve_time time)
  | UnresolvedType (var,_) ->
    (tenv,varm,senv,env), TFailure (Some var, Variable.get_locations var,
      "the type inferred is not fully resolved",
      retrieve_time time)

let treat_sig envs (annot,elem) =
  match elem with
  | Ast.Types _ | Ast.AbsType _ | Ast.SigDef _ -> treat envs (annot,elem)
  | Ast.Command _ | Ast.Definitions _ -> envs, TDone
let treat_def envs (annot,elem) =
  match elem with
  | Ast.Types _ | Ast.AbsType _ | Ast.SigDef _ -> envs, TDone
  | Ast.Command _ | Ast.Definitions _ -> treat envs (annot,elem)
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

let builtin_functions =
  let arith_operators_typ =
    mk_arrow int_typ (mk_arrow int_typ int_typ) |> TyScheme.mk_poly
  in
  [
    ("+", arith_operators_typ) ;
    ("-", arith_operators_typ) ;
    ("*", arith_operators_typ) ;
    ("/", arith_operators_typ) ;
    ("%", arith_operators_typ)
  ]

let initial_varm =
  builtin_functions |> List.fold_left (fun varm (name, _) ->
    let var = Variable.create_let (Some name) in
    StrMap.add name var varm
  ) Ast.empty_name_var_map

let initial_env =
  builtin_functions |> List.fold_left (fun env (name, t) ->
    let var = StrMap.find name initial_varm in
    Env.add var t env
  ) Env.empty

let initial_senv = VarMap.empty

let initial_tenv = empty_tenv

type parsing_result =
| PSuccess of Ast.parser_program
| PFailure of Position.t * string

let parse f =
  try
    let p = match f with
      | `File fn -> parse_program_file fn
      | `String s -> parse_program_string s
    in
    PSuccess p
  with
  | Ast.LexicalError(pos, msg) -> PFailure (pos, msg)
  | Ast.SyntaxError (pos, msg) -> PFailure (pos, msg)

