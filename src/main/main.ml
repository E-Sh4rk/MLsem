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
let is_inst_of_annot mono typ tya =
  let (tvs, mty) = TyScheme.get typ in
  let tvs = TVarSet.union tvs (TVarSet.diff (vars mty) mono) in
  let gty = TyScheme.mk tvs mty in
  TyScheme.leq_inst gty tya
let sig_to_tyscheme mono sigs =
  sigs |> conj |> TyScheme.mk_poly_except mono
let type_check_def env sigs (var,e,typ_annot) =
  let time0 = Unix.gettimeofday () in
  let retrieve_time () =
    let time1 = Unix.gettimeofday () in
    (time1 -. time0 ) *. 1000.
  in
  try
    let e = System.Ast.from_parser_ast e in
    let e = Partition.infer env e in
    let annot =
      match Reconstruction.infer env e with
      | None -> raise (Checker.Untypeable (fst e, "Annotation reconstruction failed."))
      | Some annot-> annot
    in
    let typ = Checker.typeof_def env annot e |> TyScheme.simplify in
    let mono = Env.tvars env in
    let typ =
      match typ_annot with
      | None -> typ
      | Some tya ->
        if is_inst_of_annot mono typ tya
        then Checker.generalize ~e env tya
        else raise (IncompatibleType typ)
    in
    let typ =
      match sigs with
      | None -> typ
      | Some (sigs, typ') ->
        if sigs |> List.for_all (TyScheme.leq_inst typ)
        then typ'
        else raise (IncompatibleType typ)
    in
    if TVarSet.diff (TyScheme.fv typ) mono |> TVarSet.is_empty |> not
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

let sigs_of_def varm senv env str =
  match StrMap.find_opt str varm with
  | None -> None
  | Some v ->
    begin match VarMap.find_opt v senv with
    | None -> None
    | Some sigs -> Some (sigs, Env.find v env)
    end

let treat (tenv,varm,senv,env) (annot, elem) =
  let pos = [Position.position annot] in
  try  
    match elem with
    | Ast.Definition (name, expr, tyo) ->
      let sigs = sigs_of_def varm senv env name in
      let var = Variable.create_let (Some name) in
      Variable.attach_location var (Position.position annot) ;
      begin try
        let tyo = match tyo with
        | None -> None
        | Some expr -> let (t, _) = type_expr_to_typ tenv empty_vtenv expr in Some t
        in
        let expr = Ast.parser_expr_to_expr tenv empty_vtenv varm expr in
        match type_check_def env sigs (var,expr,tyo) with
        | TCSuccess (ty,f) ->
          let varm = StrMap.add name var varm in
          let env = Env.add var ty env in
          (tenv,varm,senv,env), TSuccess (var,ty,f)
        | TCFailure (pos,msg,f) -> (tenv,varm,senv,env), TFailure (Some var,pos,msg,f)
      with
      | Ast.SymbolError msg -> (tenv,varm,senv,env), TFailure (Some var, pos, msg, 0.0)
      end
    | Ast.SigDef (name, ty) ->
      let sigs = match sigs_of_def varm senv env name with
      | None -> []
      | Some (sigs,_) -> sigs
      in
      let v = Variable.create_let (Some name) in
      Variable.attach_location v (Position.position annot) ;
      let varm = StrMap.add name v varm in
      let (ty, _) = type_expr_to_typ tenv empty_vtenv ty in
      let sigs = ty::sigs in
      let ty = sig_to_tyscheme (Env.tvars env) sigs in
      let senv = VarMap.add v sigs senv in
      let env = Env.add v ty env in
      (tenv,varm,senv,env), TDone
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
  ) System.Ast.initial_env

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

