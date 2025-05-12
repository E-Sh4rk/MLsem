open Parsing.IO
open System
open Types.Base
open Types
open Types.Additions
open Parsing
open Parsing.Variable
open Env

type def = Variable.t * Ast.expr * typ option

type typecheck_result =
| TSuccess of TyScheme.t * Env.t * float
| TFailure of (Position.t list) * string * float

exception IncompatibleType of TyScheme.t
let type_check_def env (var,e,typ_annot) =
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
    let typ =
      match typ_annot with
      | None -> typ
      | Some tya ->
        if TyScheme.leq_inst typ tya
        then TyScheme.mk_poly_except (Env.tvars env) tya
        else raise (IncompatibleType typ)
    in
    let env = Env.add var typ env in
    TSuccess (typ, env, retrieve_time ())
  with
  | Checker.Untypeable (_, str) ->
    (* TODO: retrieve pos of exprid *)
    TFailure ([], str, retrieve_time ())
  | IncompatibleType _ ->
    TFailure (Variable.get_locations var,
      "the type inferred is not a subtype of the type specified",
      retrieve_time ())

type parsing_result =
| PSuccess of type_env * (def list)
| PFailure of Position.t * string

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

let parse_and_resolve f varm =
  let last_pos = ref Position.dummy in
  try
    let ast =
      match f with
        `File fn -> parse_program_file fn
      | `String s -> parse_program_string s
    in
    let treat_elem (tenv,varm,defs) (annot, elem) =
      last_pos := Position.position annot ;
      match elem with
      | Ast.Definition (name, expr, tyo) ->
        let tyo = match tyo with
        | None -> None
        | Some expr -> let (t, _) = type_expr_to_typ tenv empty_vtenv expr in Some t
        in
        let expr = Ast.parser_expr_to_expr tenv empty_vtenv varm expr in
        let var = Variable.create_let (Some name) in
        Variable.attach_location var (Position.position annot) ;
        let varm = StrMap.add name var varm in
        (tenv,varm,(var,expr,tyo)::defs)
      | Ast.Command (str, c) ->
        (* TODO: dont execute all commands before typing *)
        begin match str, c with
        | "log", Int i -> Config.log_level := Z.to_int i
        | "value_restriction", Bool b -> Config.value_restriction := b
        | _ -> failwith ("Invalid command "^str)
        end ;
        (tenv,varm,defs)
      | Ast.Types lst ->
        let tenv = define_types tenv empty_vtenv lst in
        (tenv,varm,defs)
      | Ast.AbsType (name, vs) ->
        let tenv = define_abstract tenv name vs in
        (tenv,varm,defs)
    in
    let (tenv, _, defs) =
      List.fold_left treat_elem (empty_tenv, varm, []) ast in
    PSuccess (tenv, List.rev defs)
  with
    | Ast.LexicalError(pos, msg) -> PFailure (pos, msg)
    | Ast.SyntaxError (pos, msg) -> PFailure (pos, msg)
    | Ast.SymbolError msg -> PFailure (!last_pos, msg)
    | TypeDefinitionError msg -> PFailure (!last_pos, msg)
