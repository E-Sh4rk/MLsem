open Types.Base
open Types.Additions
open Variable

exception SymbolError of string
exception LexicalError of Position.t * string
exception SyntaxError of Position.t * string

type varname = string
type exprid = int
[@@deriving show]

type annotation = exprid Position.located

module Zd = struct
    type t = Z.t
    let pp = Z.pp_print
    let compare = Z.compare
end
type const =
| Unit | Nil
| EmptyRecord
| Bool of bool
| Int of Zd.t
| Float of float
| Char of char
| String of string
[@@deriving show, ord]

type projection = Pi of int * int | Field of string | Hd | Tl | PiTag of tag
[@@deriving show, ord]

type 'typ dom_annot = DNoAnnot | DAnnot of 'typ list
[@@deriving show, ord]
type 'typ part_annot = PNoAnnot | PAnnot of 'typ list
[@@deriving show, ord]

type ('a, 'typ, 'tag, 'v) pattern =
| PatType of 'typ
| PatVar of 'v
| PatTag of 'tag * ('a, 'typ, 'tag, 'v) pattern
| PatAnd of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatOr of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatTuple of ('a, 'typ, 'tag, 'v) pattern list
| PatCons of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatRecord of (string * (('a, 'typ, 'tag, 'v) pattern)) list * bool
| PatAssign of 'v * const
[@@deriving ord]

and ('a, 'typ, 'ato, 'tag, 'v) ast =
| Abstract of 'typ
| Const of const
| Var of 'v
| Atom of 'ato
| Tag of 'tag * ('a, 'typ, 'ato, 'tag, 'v) t
| Lambda of 'v * 'typ dom_annot * ('a, 'typ, 'ato, 'tag, 'v) t
| Fixpoint of ('a, 'typ, 'ato, 'tag, 'v) t
| Ite of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ * ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| App of ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Let of 'v * 'typ part_annot * ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Tuple of ('a, 'typ, 'ato, 'tag, 'v) t list
| Cons of ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Projection of projection * ('a, 'typ, 'ato, 'tag, 'v) t
| RecordUpdate of ('a, 'typ, 'ato, 'tag, 'v) t * string * ('a, 'typ, 'ato, 'tag, 'v) t option
| TypeConstr of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ
| TypeCoerce of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ
| PatMatch of ('a, 'typ, 'ato, 'tag, 'v) t * (('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'ato, 'tag, 'v) t) list
[@@deriving ord]

and ('a, 'typ, 'ato, 'tag, 'v) t = 'a * ('a, 'typ, 'ato, 'tag, 'v) ast

type expr = (exprid, typ, atom, tag, Variable.t) t
type parser_expr = (annotation, type_expr, string, string, varname) t

type name_var_map = Variable.t StrMap.t
let empty_name_var_map = StrMap.empty

let unique_exprid =
    let last_id = ref 0 in
    fun _ -> (
        last_id := !last_id + 1 ;
        !last_id
    )
let new_annot p =
    Position.with_pos p (unique_exprid ())
let copy_annot a =
    new_annot (Position.position a)

let dummy_pat_var_str = "_"
let dummy_pat_var =
    Variable.create_gen (Some dummy_pat_var_str)

let only_user_vars t =
    let open Types.Tvar in
    vars_internal t |> TVarSet.is_empty

(* TODO: associate a location to each exprid using a hashtbl *)
let parser_expr_to_expr tenv vtenv name_var_map e =
    let rec aux vtenv env ((exprid,pos),e) =
        let e = match e with
        | Abstract t ->
            let (t, _) = type_expr_to_typ tenv vtenv t in
            Abstract t
        | Const c -> Const c
        | Var str ->
            if StrMap.mem str env
            then Var (StrMap.find str env)
            else raise (SymbolError ("undefined symbol "^str))
        | Atom str -> Atom (get_atom tenv str)
        | Tag (str, e) -> Tag (get_tag tenv str, aux vtenv env e)
        | Lambda (str,a,e) ->
            let a, vtenv = match a with
            | DNoAnnot -> DNoAnnot, vtenv
            | DAnnot ts ->
                let (ts, vtenv) = type_exprs_to_typs tenv vtenv ts in
                DAnnot (ts), vtenv
            in
            let var = Variable.create_lambda (Some str) in
            Variable.attach_location var pos ;
            let env = StrMap.add str var env in
            Lambda (var, a, aux vtenv env e)
        | Fixpoint e -> Fixpoint (aux vtenv env e)
        | Ite (e, t, e1, e2) ->
            let (t, vtenv) = type_expr_to_typ tenv vtenv t in
            if is_test_type t
            then Ite (aux vtenv env e, t, aux vtenv env e1, aux vtenv env e2)
            else raise (SymbolError ("typecases must use a valid test type"))
        | App (e1, e2) -> App (aux vtenv env e1, aux vtenv env e2)
        | Let (str, a, e1, e2) ->
            let a, vtenv = match a with
            | PNoAnnot -> PNoAnnot, vtenv
            | PAnnot ts ->
                let (ts, vtenv) = type_exprs_to_typs tenv vtenv ts in
                PAnnot ts, vtenv
            in
            let var = Variable.create_let (Some str) in
            Variable.attach_location var pos ;
            let env' = StrMap.add str var env in
            Let (var, a, aux vtenv env e1, aux vtenv env' e2)
        | Tuple es ->
            Tuple (List.map (aux vtenv env) es)
        | Cons (e1, e2) ->
            Cons (aux vtenv env e1, aux vtenv env e2)
        | Projection (p, e) -> Projection (p, aux vtenv env e)
        | RecordUpdate (e1, l, e2) ->
            RecordUpdate (aux vtenv env e1, l, Option.map (aux vtenv env) e2)
        | TypeConstr (e, ty) ->
            let (ty, vtenv) = type_expr_to_typ tenv vtenv ty in
            if is_test_type ty
            then TypeConstr (aux vtenv env e, ty)
            else raise (SymbolError ("type constraint should be a test type"))
        | TypeCoerce (e, ty) ->
            let (ty, vtenv) = type_expr_to_typ tenv vtenv ty in
            if only_user_vars ty
            then TypeCoerce (aux vtenv env e, ty)
            else raise (SymbolError ("type coercion should not have non-user type variables"))
        | PatMatch (e, pats) ->
            PatMatch (aux vtenv env e, List.map (aux_pat pos vtenv env) pats)
        in
        (exprid,e)
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
                else raise (SymbolError ("typecases must use a valid test type"))
            | PatVar str ->
                if String.equal str dummy_pat_var_str
                then (PatVar dummy_pat_var, vtenv, StrMap.empty)
                else
                    let var = find_or_def_var str in
                    (PatVar var, vtenv, StrMap.singleton str var)
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
| Definition of (string * parser_expr * type_expr option)
| Types of (string * string list * type_expr) list
| AbsType of string * variance list
| Command of string * const

type parser_program = (annotation * parser_element) list
