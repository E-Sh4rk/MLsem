open Mlsem_common
open Mlsem_types
open Mlsem_system.Ast
open IO
open Builder
module MVariable = Mlsem_lang.MVariable
module NameMap = PAst.NameMap

exception IncompatibleType of Variable.t * TyScheme.t
exception UnresolvedType of Variable.t * TyScheme.t
exception Untypeable of Variable.t option * Mlsem_system.Checker.error
exception AlreadyDefined of Variable.t

(* Utility *)

let retrieve_time time =
  let time' = Unix.gettimeofday () in
  (time' -. time) *. 1000.

let check_resolved ~allow_mono var env typ =
  let allowed = if allow_mono then Env.tvars env else MVarSet.empty in
  if MVarSet.subset (TyScheme.fv typ) allowed |> not
  then raise (UnresolvedType (var,typ))

let resolve_kind benv kind =
  match kind with
  | PAst.Immut -> MVariable.Immut, benv | PAst.Mut -> MVariable.Mut, benv
  | PAst.AnnotMut ty ->
    let ty, benv = type_expr_to_gty benv ty in
    MVariable.AnnotMut ty, benv

let sigs_of_def varm senv env (kind,str) =
  match NameMap.find_opt str varm with
  | None ->
    let var = MVariable.create kind (Some str) in
    var, None
  | Some v ->
    begin match VarMap.find_opt v senv with
    | None -> raise (AlreadyDefined v)
    | Some sigs ->
      if MVariable.kind_compat kind (MVariable.kind v) |> not then raise (AlreadyDefined v) ;
      v, Some (sigs, Env.find v env)
    end

(* Type checking and inference *)

let infer var env e =
  let annot =
    (* Format.printf "@.@.%a@.@." Mlsem_system.Ast.pp e ; *)
    let narrowing = !Config.type_narrowing in
    let r =
      if narrowing <> NoNarrowing
      then Mlsem_system.Refinement.refinements env e
      else Mlsem_system.Refinement.Refinements.empty
    in
    (* REnvSet.elements r |> List.iter (fun renv -> Format.printf "Renv: %a@." REnv.pp renv) ; *)
    try Mlsem_system.Reconstruction.infer
      ~direct_narrowing:(narrowing = DirectNarrowing || narrowing = BothNarrowing)
      ~partition_narrowing:(narrowing = PartitionNarrowing || narrowing = BothNarrowing)
      env r e with
    | Mlsem_system.Checker.Untypeable err ->
      (* Format.printf "@.@.%a@.@." Mlsem_system.Ast.pp e ; *)
      raise (Untypeable (var, err))
  in
  let ty = Mlsem_system.Checker.typeof_def env annot e in
  let (tvs, ty) = TyScheme.get ty in
  let ty = TyScheme.mk tvs (GTy.ub ty |> GTy.mk) |> Signature.simplify_tyscheme in
  let msg = Mlsem_system.Analyzer.analyze e annot in
  ty, msg

let type_check_with_sigs env (var,e,sigs,aty) =
  let e, id = Transform.expr_to_ast e, Eid.refresh (fst e) in
  let c = if !Config.allow_implicit_downcast then CheckStatic else Check in
  let es = sigs |> List.concat_map (Signature.decompose ~recursive:true) |> List.map (fun s ->
    id, TypeCoerce (e, Signature.to_gty s, c)
    ) |> List.map push_coercions in
  let tys, msg = List.map (infer (Some var) env) es |> List.split in
  List.iter (check_resolved ~allow_mono:false var env) tys ;
  let msg = (List.concat msg)@(Mlsem_system.Analyzer.get_unreachable e) in
  (var,(aty,sigs)),msg

let type_check_recs pos env lst =
  let e =
    Eid.unique_with_pos pos,
    PAst.LambdaRec (List.map (fun (v,e) -> (v,None,e)) lst) in
  let e = Transform.expr_to_ast e |> push_coercions in
  let ty, msg = infer None env e in
  let msg = msg@(Mlsem_system.Analyzer.get_unreachable e) in
  let tvs, ty = ty |> TyScheme.get in
  let n = List.length lst in
  List.mapi (fun i (var,_) ->
    let ty = GTy.map (Tuple.proj n i) ty |> TyScheme.mk tvs |> Signature.simplify_tyscheme in
    check_resolved ~allow_mono:true var env ty ;
    (var, (ty, Signature.of_tyscheme ty))
  ) lst, msg

type message = Mlsem_system.Analyzer.severity * Position.t * string * string option
type inferred = {
  var: Variable.t;
  ty: TyScheme.t;
  sigs: Signature.t;
  declared: bool;
}
type treat_result =
| TSuccess of inferred list * message list * float
| TDone
| TFailure of Variable.t option * Position.t * string * string option * float

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
        let kind, benv' = resolve_kind !benv kind in
        let var, sigs = sigs_of_def !varm senv env (kind, name) in
        benv := benv' ; Variable.attach_location var (Position.position annot) ;
        varm := NameMap.add name var !varm ;
        (var, e, sigs)
      )
      in
      let varm = !varm in
      let sigs, recs = List.partition_map (fun (var, e, sigs) ->
        v := var ;
        let e, benv' = PAst.to_expr !benv varm e in
        benv := benv' ;
        match sigs with
        | None -> Either.Right (var, e)
        | Some (sigs,aty) -> Either.Left (var, e, sigs, aty)
        ) lst in
      let tys1, msg1 = type_check_recs pos env recs in
      let env = List.fold_left (fun env (v,(ty,_)) ->
        try MVariable.add_to_env v ty env
        with Invalid_argument _ -> raise (IncompatibleType (v,ty))
      ) env tys1 in
      let tys2, msg2 = List.map (type_check_with_sigs env) sigs |> List.split in
      let msg = msg1@(List.concat msg2) |> List.map (fun r ->
        (r.Mlsem_system.Analyzer.severity, Eid.loc r.eid, r.title, r.descr)
      ) in
      let senv = List.fold_left (fun senv (v,_) -> VarMap.remove v senv) senv tys2 in
      (* [tys1] are the inferred (signature-less) bindings; [tys2] carry a
         user-written [val] declaration. [declared] lets the LSP suppress the
         inline-signature action where a declaration already exists. *)
      let render ~declared (v, (ty,sigs)) = { var = v; ty; sigs; declared } in
      let tys = List.map (render ~declared:false) tys1 @ List.map (render ~declared:true) tys2 in
      (!benv,varm,senv,env), TSuccess (tys,msg,retrieve_time time)
    | PAst.SigDef (name, mut, ty) ->
      begin try
        let new_sig, benv = Signature.build benv ty in
        let kind = if mut then MVariable.AnnotMut (Signature.to_gty new_sig) else Immut in
        let var, sigs = sigs_of_def varm senv env (kind, name) in
        Variable.attach_sig_location var (Position.position annot) ;
        let varm = NameMap.add name var varm in
        let sigs = match sigs with
        | None when Env.mem var env ->
          invalid_arg "A type annotation must precede the definition."
        | None (* First definition *) -> [new_sig]
        | Some (sigs, _) -> sigs@[new_sig]
        in
        let ty = Signature.to_tyscheme sigs in
        let env = MVariable.replace_in_env var ty env in
        let senv = VarMap.add var sigs senv in
        (benv,varm,senv,env), TDone
      with Invalid_argument str -> (benv,varm,senv,env),
        TFailure (None, pos, str, None, 0.0)
      end
    | PAst.Command (str, c) ->
      begin match str, c with
      | "value_restriction", Bool b -> Config.value_restriction := b
      | "type_narrowing", String "partition" -> Config.type_narrowing := PartitionNarrowing
      | "type_narrowing", String "direct" -> Config.type_narrowing := DirectNarrowing
      | "type_narrowing", Bool false | "type_narrowing", String "no"
      -> Config.type_narrowing := NoNarrowing
      | "type_narrowing", Bool true | "type_narrowing", String "yes"
      -> Config.type_narrowing := BothNarrowing
      | "allow_implicit_downcast", Bool b -> Config.allow_implicit_downcast := b
      | "infer_overload", Bool b -> Config.infer_overload := b
      | "normalization", Bool false | "normalization", String "no" ->
        Config.normalization_fun := Fun.id
      | "normalization", String "no_empty_param" ->
        Config.normalization_fun := Mlsem_system.Heuristics.normalize_empty_abstracts
      | "subst_normalization", Bool false | "subst_normalization", String "no" ->
        Config.subst_normalization_fun := (fun _ x -> x)
      | "subst_normalization", String "no_abstract_inter" ->
        Config.subst_normalization_fun := Mlsem_system.Heuristics.normalize_abstract_factors
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
      Some (Format.asprintf "type inferred: @[<hov>%a@]" TyScheme.pp_short ty),
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
type envs = Builder.benv * Variable.t NameMap.t * Signature.t VarMap.t * Env.t * PEnv.t

let print_ty pp (_,_,_,_,penv) ty =
  PEnv.sequential_handler penv
    (fun () -> Format.asprintf "@[<hov>%a@]" pp ty) ()
  |> fst
let display envs ty = print_ty TyScheme.pp_short envs ty
let signature envs sigs = sigs |> List.map (print_ty Signature.pp_overload envs)

type parsing_result =
| PSuccess of PAst.program
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
