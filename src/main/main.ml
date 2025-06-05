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

type typecheck_result =
| TCSuccess of TyScheme.t * float
| TCFailure of (Position.t list) * string * float

exception IncompatibleType of TyScheme.t
exception UnresolvedType of TyScheme.t
let sigs_to_tyscheme mono sigs =
  if sigs |> List.for_all (fun ty -> vars_internal ty |> TVarSet.is_empty) then
    Some (sigs |> conj |> TyScheme.mk_poly_except mono)
  else None
(* TODO: support for multiple definitions *)
let type_check_def env (sigs,aty) (var,e) =
  let env = Env.rm var env in
  let time0 = Unix.gettimeofday () in
  let retrieve_time () =
    let time1 = Unix.gettimeofday () in
    (time1 -. time0 ) *. 1000.
  in
  try
    let lambdarec oty e =
      Parsing.Ast.unique_exprid (), Parsing.Ast.LambdaRec [(var,oty,e)]
    in
    let coerce ty e =
      Parsing.Ast.unique_exprid (), Parsing.Ast.TypeCoerce (e,ty)
    in
    let es = match sigs with
    | [] -> [lambdarec None e]
    | sigs -> List.map (fun s -> lambdarec (Some s) e |> coerce (mk_tuple [s])) sigs
    in
    let infer e =
      let e = System.Ast.from_parser_ast e in
      let e = Partition.infer env e in
      let annot =
        match Reconstruction.infer env e with
        | None ->
          (* Format.printf "@.@.%a@.@." System.Ast.pp e ; *)
          raise (Checker.Untypeable (fst e, "Annotation reconstruction failed."))
        | Some annot-> annot
      in
      let tvs, ty = Checker.typeof_def env annot e |> TyScheme.simplify |> TyScheme.get in
      TyScheme.mk tvs (pi 1 0 ty)
    in
    let typs = List.map infer es in
    let tscap t1 t2 =
      let (tvs1, t1), (tvs2, t2) = TyScheme.get t1, TyScheme.get t2 in
      TyScheme.mk (TVarSet.union tvs1 tvs2) (cap t1 t2)
    in
    let typ = List.fold_left tscap (TyScheme.mk_mono any) typs in
    if TyScheme.leq typ aty |> not then raise (IncompatibleType typ) ;
    if TVarSet.diff (TyScheme.fv typ) (Env.tvars env) |> TVarSet.is_empty |> not
    then raise (UnresolvedType typ) ;
    TCSuccess (typ, retrieve_time ())
  with
  | Checker.Untypeable (_, str) ->
    (* TODO: retrieve pos of exprid *)
    TCFailure ([], str, retrieve_time ())
  | IncompatibleType _ ->
    TCFailure (Variable.get_locations var,
      "the type inferred is not a subtype of the type specified",
      retrieve_time ())
  | UnresolvedType _ ->
    TCFailure (Variable.get_locations var,
      "the type inferred is not fully resolved",
      retrieve_time ())

type 'a treat_result =
| TSuccess of Variable.t * TyScheme.t * float
| TDone
| TFailure of Variable.t option * (Position.t list) * string * float

exception AlreadyDefined of Variable.t

let check_not_defined varm env str =
  match StrMap.find_opt str varm with
  | None -> ()
  | Some v ->
    if Env.mem v env then raise (AlreadyDefined (StrMap.find str varm))

let sigs_of_def varm senv env str oty =
  let v, sigs, aty =
    match StrMap.find_opt str varm with
    | None ->
      let var = Variable.create_let (Some str) in
      var, [], TyScheme.mk_mono any
    | Some v ->
      begin match VarMap.find_opt v senv with
      | None -> raise (AlreadyDefined v)
      | Some sigs -> v, sigs, Env.find v env
      end
  in
  let sigs = match sigs, oty with
  | sigs, None -> sigs
  | [], Some ty -> [ty]
  | sigs, Some _ -> sigs
  in
  v, sigs, aty

let treat (tenv,varm,senv,env) (annot, elem) =
  let pos = [Position.position annot] in
  try  
    match elem with
    | Ast.Definitions [(name, oty, expr)] ->
      let oty, vtenv = match oty with
      | None -> None, empty_vtenv
      | Some ty ->
        let (ty, vtenv) = type_expr_to_typ tenv empty_vtenv ty in
        Some ty, vtenv
      in
      let var, sigs, aty = sigs_of_def varm senv env name oty in
      Variable.attach_location var (Position.position annot) ;
      let varm = StrMap.add name var varm in
      begin try
        let expr = Ast.parser_expr_to_expr tenv vtenv varm expr in
        match type_check_def env (sigs,aty) (var,expr) with
        | TCSuccess (ty,f) ->
          let senv = VarMap.remove var senv in
          let env = if Env.mem var env then env else Env.add var ty env in
          (tenv,varm,senv,env), TSuccess (var,ty,f)
        | TCFailure (pos,msg,f) -> (tenv,varm,senv,env), TFailure (Some var,pos,msg,f)
      with
      | Ast.SymbolError msg -> (tenv,varm,senv,env), TFailure (Some var, pos, msg, 0.0)
      end
    | Ast.Definitions _ -> failwith "TODO"
    | Ast.SigDef (name, tys) ->
      check_not_defined varm env name ;
      let v = Variable.create_let (Some name) in
      Variable.attach_location v (Position.position annot) ;
      let (sigs, _) = type_exprs_to_typs tenv empty_vtenv tys in
      begin match sigs_to_tyscheme (Env.tvars env) sigs with
      | None -> (tenv,varm,senv,env),
        TFailure (Some v, pos, "Signatures cannot contain weak variables.", 0.0)
      | Some ty ->
        let varm = StrMap.add name v varm in
        let senv = VarMap.add v sigs senv in
        let env = Env.add v ty env in
        (tenv,varm,senv,env), TDone
      end
    | Ast.Command (str, c) ->
      begin match str, c with
      | "log", Int i -> Config.log_level := Z.to_int i
      | "value_restriction", Bool b -> Config.value_restriction := b
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
  | TypeDefinitionError msg -> (tenv,varm,senv,env), TFailure (None, pos, msg, 0.0)
  | AlreadyDefined v ->
    (tenv,varm,senv,env), TFailure (Some v, pos, "Symbol already defined.", 0.0)

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

