open Common
open Types
open Types.Builder
open System.Ast

exception SymbolError of string
exception LexicalError of Position.t * string
exception SyntaxError of Position.t * string

type varname = string
type annotation = Eid.t Position.located

type 'typ lambda_annot = 'typ option

type ('a, 'typ, 'tag, 'v) pattern =
| PatType of 'typ
| PatVar of 'v
| PatLit of const
| PatTag of 'tag * ('a, 'typ, 'tag, 'v) pattern
| PatAnd of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatOr of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatTuple of ('a, 'typ, 'tag, 'v) pattern list
| PatCons of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatRecord of (string * (('a, 'typ, 'tag, 'v) pattern)) list * bool
| PatAssign of 'v * const

and ('a, 'typ, 'enu, 'tag, 'v) ast =
| Abstract of 'typ
| Const of const
| Var of 'v
| Enum of 'enu
| Tag of 'tag * ('a, 'typ, 'enu, 'tag, 'v) t
| Suggest of 'v * 'typ list * ('a, 'typ, 'enu, 'tag, 'v) t
| Lambda of 'v * 'typ lambda_annot * ('a, 'typ, 'enu, 'tag, 'v) t
| LambdaRec of ('v * 'typ option * ('a, 'typ, 'enu, 'tag, 'v) t) list
| Ite of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ * ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| App of ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| Let of 'v * ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| Tuple of ('a, 'typ, 'enu, 'tag, 'v) t list
| Cons of ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| Projection of projection * ('a, 'typ, 'enu, 'tag, 'v) t
| RecordUpdate of ('a, 'typ, 'enu, 'tag, 'v) t * string * ('a, 'typ, 'enu, 'tag, 'v) t option
| TypeCast of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ
| TypeCoerce of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ option * coerce
| PatMatch of ('a, 'typ, 'enu, 'tag, 'v) t * (('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'enu, 'tag, 'v) t) list
| Cond of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ * ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t option
| While of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ * ('a, 'typ, 'enu, 'tag, 'v) t
| Seq of ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t

and ('a, 'typ, 'enu, 'tag, 'v) t = 'a * ('a, 'typ, 'enu, 'tag, 'v) ast

type expr = (Eid.t, Ty.t, Enum.t, Tag.t, Variable.t) t
type parser_expr = (annotation, type_expr, string, string, varname) t

type name_var_map = Variable.t StrMap.t
let empty_name_var_map = StrMap.empty

let new_annot p =
    Position.with_pos p (Eid.unique_with_pos p)

let dummy_pat_var_str = "_"
let dummy_pat_var =
    Variable.create_gen (Some dummy_pat_var_str)

let parser_expr_to_expr tenv vtenv name_var_map e =
    let aux_a tyo vtenv =
        match tyo with
        | None -> None, vtenv
        | Some ty ->
            let (ty, vtenv) = type_expr_to_typ tenv vtenv ty in
            Some ty, vtenv
    in
    let aux_cond tenv vtenv t =
        let (t, vtenv) = type_expr_to_typ tenv vtenv t in
        if is_test_type t
        then (t, vtenv)
        else raise (SymbolError ("typecases should use test types"))
    in
    let aux_var env str =
        if StrMap.mem str env
        then StrMap.find str env
        else raise (SymbolError ("undefined symbol "^str))
    in
    let rec aux vtenv env ((eid,pos),e) =
        let e = match e with
        | Abstract t ->
            let (t, _) = type_expr_to_typ tenv vtenv t in
            Abstract t
        | Const c -> Const c
        | Var str -> Var (aux_var env str)
        | Enum str -> Enum (get_enum tenv str)
        | Tag (str, e) -> Tag (get_tag tenv str, aux vtenv env e)
        | Suggest (str,tys,e) ->
            let tys, vtenv = type_exprs_to_typs tenv vtenv tys in
            let var = aux_var env str in
            Suggest (var, tys, aux vtenv env e)
        | Lambda (str,da,e) ->
            let da, vtenv = aux_a da vtenv in
            let var = Variable.create_lambda (Some str) in
            Variable.attach_location var pos ;
            let env = StrMap.add str var env in
            Lambda (var, da, aux vtenv env e)
        | LambdaRec lst ->
            let aux (str,tyo,e) =
                let var = Variable.create_lambda (Some str) in
                Variable.attach_location var pos ;
                let env = StrMap.add str var env in
                let a, vtenv = aux_a tyo vtenv in
                var, a, aux vtenv env e
            in 
            LambdaRec (List.map aux lst)
        | Ite (e, t, e1, e2) ->
            let (t, vtenv) = aux_cond tenv vtenv t in
            Ite (aux vtenv env e, t, aux vtenv env e1, aux vtenv env e2)
        | App (e1, e2) -> App (aux vtenv env e1, aux vtenv env e2)
        | Let (str, e1, e2) ->
            let var = Variable.create_let (Some str) in
            Variable.attach_location var pos ;
            let env' = StrMap.add str var env in
            Let (var, aux vtenv env e1, aux vtenv env' e2)
        | Tuple es ->
            Tuple (List.map (aux vtenv env) es)
        | Cons (e1, e2) ->
            Cons (aux vtenv env e1, aux vtenv env e2)
        | Projection (p, e) -> Projection (p, aux vtenv env e)
        | RecordUpdate (e1, l, e2) ->
            RecordUpdate (aux vtenv env e1, l, Option.map (aux vtenv env) e2)
        | TypeCast (e, ty) ->
            let (ty, vtenv) = type_expr_to_typ tenv vtenv ty in
            if is_test_type ty
            then TypeCast (aux vtenv env e, ty)
            else raise (SymbolError ("type constraint should be a test type"))
        | TypeCoerce (e, Some ty, c) ->
            let (ty, vtenv) = type_expr_to_typ tenv vtenv ty in
            TypeCoerce (aux vtenv env e, Some ty, c)
        | TypeCoerce (e, None, c) -> TypeCoerce (aux vtenv env e, None, c)
        | PatMatch (e, pats) ->
            PatMatch (aux vtenv env e, List.map (aux_pat pos vtenv env) pats)
        | Cond (e, t, e1, e2) ->
            let (t, vtenv) = aux_cond tenv vtenv t in
            Cond (aux vtenv env e, t, aux vtenv env e1, Option.map (aux vtenv env) e2)
        | While (e, t, e') ->
            let (t, vtenv) = aux_cond tenv vtenv t in
            While (aux vtenv env e, t, aux vtenv env e')
        | Seq (e1, e2) -> Seq (aux vtenv env e1, aux vtenv env e2)
        in
        (eid,e)
    and aux_pat pos vtenv env (pat, e) =
        let merge_disj =
            StrMap.union (fun str v1 v2 ->
                if Variable.equals v1 v2 then Some v1
                else raise (SymbolError ("matched variables "^str^" are conflicting")))
        in
        let rec aux_p vtenv env pat =
            let find_or_def_var str =
                if StrMap.mem str env
                then StrMap.find str env
                else
                    let var = Variable.create_let (Some str) in
                    Variable.attach_location var pos ;
                    var
            in
            match pat with
            | PatType t ->
                let (t, vtenv) = type_expr_to_typ tenv vtenv t in
                if is_test_type t
                then (PatType t, vtenv, StrMap.empty)
                else raise (SymbolError ("typecases should use test types"))
            | PatVar str ->
                if String.equal str dummy_pat_var_str
                then (PatVar dummy_pat_var, vtenv, StrMap.empty)
                else
                    let var = find_or_def_var str in
                    (PatVar var, vtenv, StrMap.singleton str var)
            | PatLit c -> (PatLit c, vtenv, StrMap.empty)
            | PatTag (str, p) ->
                let tag = get_tag tenv str in
                let (p, vtenv, env) = aux_p vtenv env p in
                (PatTag (tag, p), vtenv, env)
            | PatAnd (p1, p2) ->
                let (p1, vtenv, env1) = aux_p vtenv env p1 in
                let (p2, vtenv, env2) = aux_p vtenv env p2 in
                (PatAnd (p1, p2), vtenv, merge_disj env1 env2)
            | PatOr (p1, p2) ->
                let (p1, vtenv, env1) = aux_p vtenv env p1 in
                let env = merge_disj env env1 in 
                let (p2, vtenv, env2) = aux_p vtenv env p2 in
                if StrMap.equal (Variable.equals) env1 env2 |> not
                then raise (SymbolError ("missing matched variables in pattern")) ;
                (PatOr (p1, p2), vtenv, env1)
            | PatTuple ps ->
                let aux (ps, vtenv, env) p =
                    let (p, vtenv, env') = aux_p vtenv env p in
                    (p::ps, vtenv, merge_disj env env')
                in
                let (ps, vtenv, env) = List.fold_left aux ([],vtenv,env) ps in
                (PatTuple (List.rev ps), vtenv, env)
            | PatCons (p1, p2) ->
                let (p1, vtenv, env1) = aux_p vtenv env p1 in
                let (p2, vtenv, env2) = aux_p vtenv env p2 in
                (PatCons (p1, p2), vtenv, merge_disj env1 env2)
            | PatRecord (fields, o) ->
                let (fields, vtenv, env) = List.fold_left
                    (fun (fields, vtenv, acc_env) (name, p) ->
                        let (p, vtenv, env') = aux_p vtenv env p in
                        ((name, p)::fields, vtenv, merge_disj acc_env env')
                ) ([], vtenv, env) fields in
                (PatRecord (List.rev fields, o), vtenv, env)
            | PatAssign (str, c) ->
                if String.equal str dummy_pat_var_str
                then raise (SymbolError "invalid variable name for a pattern assignement") ;
                let var = find_or_def_var str in
                (PatAssign (var, c), vtenv, StrMap.singleton str var)
        in
        let (pat, vtenv, env') = aux_p vtenv StrMap.empty pat in
        let env = StrMap.add_seq (StrMap.to_seq env') env in
        (pat, aux vtenv env e)
    in
    aux vtenv name_var_map e

type parser_element =
| Definitions of (string * parser_expr) list
| SigDef of string * type_expr option
| Types of (string * string list * type_expr) list
| AbsType of string * Abstract.variance list
| Command of string * const

type parser_program = (annotation * parser_element) list
