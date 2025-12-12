open Mlsem_common
open Mlsem_types
open Mlsem_system.Ast
open Mlsem_lang

exception SymbolError of string
exception LexicalError of Position.t * string
exception SyntaxError of Position.t * string

type 'typ lambda_annot = 'typ option
type 'typ vkind = Immut | AnnotMut of 'typ | Mut
type ('typ,'v) vdef = 'typ vkind * 'v

type ('a, 'typ, 'tag, 'v) pattern =
| PatType of 'typ
| PatVar of ('typ,'v) vdef
| PatLit of Const.t
| PatTag of 'tag * ('a, 'typ, 'tag, 'v) pattern
| PatAnd of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatOr of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatTuple of ('a, 'typ, 'tag, 'v) pattern list
| PatCons of ('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'tag, 'v) pattern
| PatRecord of (string * (('a, 'typ, 'tag, 'v) pattern)) list * bool
| PatAssign of ('typ,'v) vdef * Const.t

and ('a, 'typ, 'enu, 'tag, 'v) ast =
| Magic of 'typ
| Const of Const.t
| Var of 'v
| Enum of 'enu
| Tag of 'tag * ('a, 'typ, 'enu, 'tag, 'v) t
| Suggest of 'v * 'typ list * ('a, 'typ, 'enu, 'tag, 'v) t
| Lambda of 'v * 'typ lambda_annot * ('a, 'typ, 'enu, 'tag, 'v) t
| LambdaRec of ('v * 'typ lambda_annot * ('a, 'typ, 'enu, 'tag, 'v) t) list
| Ite of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ * ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| App of ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| Let of ('typ,'v) vdef * ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| Declare of ('typ,'v) vdef * ('a, 'typ, 'enu, 'tag, 'v) t
| Tuple of ('a, 'typ, 'enu, 'tag, 'v) t list
| Cons of ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| Projection of projection * ('a, 'typ, 'enu, 'tag, 'v) t
| Constructor of constructor * ('a, 'typ, 'enu, 'tag, 'v) t list
| Operation of operation * ('a, 'typ, 'enu, 'tag, 'v) t
| Record of (string * ('a, 'typ, 'enu, 'tag, 'v) t) list
| RecordUpdate of ('a, 'typ, 'enu, 'tag, 'v) t * string * ('a, 'typ, 'enu, 'tag, 'v) t option
| TypeCast of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ option * check
| TypeCoerce of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ option * check
| VarAssign of 'v * ('a, 'typ, 'enu, 'tag, 'v) t
| PatMatch of ('a, 'typ, 'enu, 'tag, 'v) t * (('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'enu, 'tag, 'v) t) list
| Cond of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ * ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t option
| While of ('a, 'typ, 'enu, 'tag, 'v) t * 'typ * ('a, 'typ, 'enu, 'tag, 'v) t
| Seq of ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| Alt of ('a, 'typ, 'enu, 'tag, 'v) t * ('a, 'typ, 'enu, 'tag, 'v) t
| Return of ('a, 'typ, 'enu, 'tag, 'v) t
| Break | Continue

and ('a, 'typ, 'enu, 'tag, 'v) t = 'a * ('a, 'typ, 'enu, 'tag, 'v) ast

type expr = (Eid.t, Ty.t, Enum.t, Tag.t, Variable.t) t

(* ===== ParserExpr ===== *)

module type ParserExpr = sig
    type texpr
    type benv
    type varname = string
    type annotation = Eid.t Position.located
    val new_annot : Position.t -> annotation

    type pexpr = (annotation, texpr, string, string, varname) t
    type pat = (annotation, texpr, string, varname) pattern

    module NameMap : Map.S with type key=string
    type name_var_map = Variable.t NameMap.t
    val empty_name_var_map : name_var_map

    val to_expr : benv -> name_var_map -> pexpr -> expr * benv

    type element =
    | Definitions of ((texpr, string) vdef * pexpr) list
    | SigDef of string * bool (* mutable *) * texpr option
    | Types of (string * string list * texpr) list
    | AbsType of string * int
    | Command of string * Const.t

    type program = (annotation * element) list
end

module ParserExpr(B:Builder'.B) = struct
    type texpr = B.type_expr
    type benv = B.benv
    type varname = string
    type annotation = Eid.t Position.located
    let new_annot p = Position.with_pos p (Eid.unique_with_pos p)

    type pexpr = (annotation, B.type_expr, string, string, varname) t
    type pat = (annotation, B.type_expr, string, varname) pattern

    module NameMap = Map.Make(String)
    type name_var_map = Variable.t NameMap.t
    let empty_name_var_map = NameMap.empty

    let to_expr benv env e =
        let benv = ref benv in
        let aux_ty ty =
            let ty, benv' = B.type_expr_to_typ !benv ty in
            benv := benv' ; ty
        in
        let aux_tys tys =
            let tys, benv' = B.type_exprs_to_typs !benv tys in
            benv := benv' ; tys
        in
        let aux_a tyo = Option.map aux_ty tyo in
        let aux_cond t =
            let t = aux_ty t in
            if B.is_test_type t then t
            else raise (SymbolError ("typecases should use test types"))
        in
        let aux_var env str =
            if NameMap.mem str env
            then NameMap.find str env
            else raise (SymbolError ("undefined symbol "^str))
        in
        let aux_vkind k =
            match k with
            | Immut -> MVariable.Immut, Immut | Mut -> MVariable.Mut, Mut
            | AnnotMut ty ->
                let ty = aux_ty ty in
                MVariable.AnnotMut ty, AnnotMut ty
        in
        let get_enum str =
            let enum, benv' = B.get_enum !benv str in
            benv := benv' ; enum
        in
        let get_tag str =
            let tag, benv' = B.get_tag !benv str in
            benv := benv' ; tag
        in
        let rec aux env ((eid,pos),e) =
            let e = match e with
            | Magic t -> Magic (aux_ty t)
            | Const c -> Const c
            | Var str -> Var (aux_var env str)
            | Enum str -> Enum (get_enum str)
            | Tag (str, e) -> Tag (get_tag str, aux env e)
            | Suggest (str,tys,e) ->
                Suggest (aux_var env str, aux_tys tys, aux env e)
            | Lambda (str,da,e) ->
                let var = MVariable.create Immut (Some str) in
                Variable.attach_location var pos ;
                let env = NameMap.add str var env in
                Lambda (var, aux_a da, aux env e)
            | LambdaRec lst ->
                let aux (str,tyo,e) =
                    let var = MVariable.create Immut (Some str) in
                    Variable.attach_location var pos ;
                    let env = NameMap.add str var env in
                    var, aux_a tyo, aux env e
                in 
                LambdaRec (List.map aux lst)
            | Ite (e, t, e1, e2) ->
                Ite (aux env e, aux_cond t, aux env e1, aux env e2)
            | App (e1, e2) -> App (aux env e1, aux env e2)
            | Let ((kind,str), e1, e2) ->
                let mkind, kind = aux_vkind kind in
                let var = MVariable.create mkind (Some str) in
                Variable.attach_location var pos ;
                let env' = NameMap.add str var env in
                Let ((kind, var), aux env e1, aux env' e2)
            | Declare ((kind,str), e) ->
                let mkind, kind = aux_vkind kind in
                let var = MVariable.create mkind (Some str) in
                assert (MVariable.is_mutable var) ;
                Variable.attach_location var pos ;
                let env' = NameMap.add str var env in
                Declare ((kind, var), aux env' e)
            | Tuple es -> Tuple (List.map (aux env) es)
            | Cons (e1, e2) -> Cons (aux env e1, aux env e2)
            | Projection (p, e) -> Projection (p, aux env e)
            | Constructor (c, es) -> Constructor (c, List.map (aux env) es)
            | Operation (o, e) -> Operation (o, aux env e)
            | Record lst -> Record (List.map (fun (str, e) -> str, aux env e) lst)
            | RecordUpdate (e1, l, e2) ->
                RecordUpdate (aux env e1, l, Option.map (aux env) e2)
            | TypeCast (e, Some ty, c) ->
                let ty = aux_ty ty in
                TypeCast (aux env e, Some ty, c)
            | TypeCast (e, None, c) -> TypeCast (aux env e, None, c)
            | TypeCoerce (e, Some ty, c) ->
                let ty = aux_ty ty in
                TypeCoerce (aux env e, Some ty, c)
            | TypeCoerce (e, None, c) -> TypeCoerce (aux env e, None, c)
            | VarAssign (str, e) -> VarAssign (aux_var env str, aux env e)
            | PatMatch (e, pats) ->
                PatMatch (aux env e, List.map (aux_pat pos env) pats)
            | Cond (e, t, e1, e2) ->
                Cond (aux env e, aux_cond t, aux env e1, Option.map (aux env) e2)
            | While (e, t, e') -> While (aux env e, aux_cond t, aux env e')
            | Seq (e1, e2) -> Seq (aux env e1, aux env e2)
            | Alt (e1, e2) -> Alt (aux env e1, aux env e2)
            | Return e -> Return (aux env e)
            | Break -> Break | Continue -> Continue
            in
            (eid,e)
        and aux_pat pos env (pat, e) =
            let merge_disj =
                NameMap.union (fun str v1 v2 ->
                    if Variable.equal v1 v2 then Some v1
                    else raise (SymbolError ("matched variables "^str^" are conflicting")))
            in
            let rec aux_p env pat =
                let find_or_def_var (kind, str) =
                    let mkind, kind = aux_vkind kind in
                    if NameMap.mem str env
                    then
                        let v = NameMap.find str env in
                        if MVariable.kind_equal (MVariable.kind v) mkind then kind, v, str
                        else raise (SymbolError ("inconsistent mutability for var '"^str^"'"))
                    else
                        let var = MVariable.create mkind (Some str) in
                        Variable.attach_location var pos ; kind, var, str
                in
                match pat with
                | PatType t ->
                    let t = aux_ty t in
                    if B.is_test_type t
                    then (PatType t, NameMap.empty)
                    else raise (SymbolError ("typecases should use test types"))
                | PatVar vdef ->
                    let mut, var, str = find_or_def_var vdef in
                    (PatVar (mut, var), NameMap.singleton str var)
                | PatLit c ->
                    if Mlsem_lang.Const.is_approximated c
                    then raise (SymbolError ("cannot pattern-match on approximated constants"))
                    else (PatLit c, NameMap.empty)
                | PatTag (str, p) ->
                    let tag = get_tag str in
                    let (p, env) = aux_p env p in
                    (PatTag (tag, p), env)
                | PatAnd (p1, p2) ->
                    let (p1, env1) = aux_p env p1 in
                    let (p2, env2) = aux_p env p2 in
                    (PatAnd (p1, p2), merge_disj env1 env2)
                | PatOr (p1, p2) ->
                    let (p1, env1) = aux_p env p1 in
                    let (p2, env2) = aux_p (merge_disj env env1) p2 in
                    if NameMap.equal (Variable.equal) env1 env2 |> not
                    then raise (SymbolError ("missing matched variables in pattern")) ;
                    (PatOr (p1, p2), env1)
                | PatTuple ps ->
                    let aux (ps, acc_env) p =
                        let (p, env') = aux_p env p in
                        (p::ps, merge_disj acc_env env')
                    in
                    let (ps, env) = List.fold_left aux ([],env) ps in
                    (PatTuple (List.rev ps), env)
                | PatCons (p1, p2) ->
                    let (p1, env1) = aux_p env p1 in
                    let (p2, env2) = aux_p env p2 in
                    (PatCons (p1, p2), merge_disj env1 env2)
                | PatRecord (fields, o) ->
                    let (fields, env) = List.fold_left
                        (fun (fields, acc_env) (name, p) ->
                            let (p, env') = aux_p env p in
                            ((name, p)::fields, merge_disj acc_env env')
                    ) ([], env) fields in
                    (PatRecord (List.rev fields, o), env)
                | PatAssign (vdef, c) ->
                    let mut, var, str = find_or_def_var vdef in
                    (PatAssign ((mut, var), c), NameMap.singleton str var)
            in
            let (pat, env') = aux_p NameMap.empty pat in
            let env = NameMap.add_seq (NameMap.to_seq env') env in
            (pat, aux env e)
        in
        let res = aux env e in res, !benv

    type element =
    | Definitions of ((texpr, string) vdef * pexpr) list
    | SigDef of string * bool (* mutable *) * texpr option
    | Types of (string * string list * texpr) list
    | AbsType of string * int
    | Command of string * Const.t

    type program = (annotation * element) list
end

module type ParserExt = sig
  module B : Mlsem_types.Builder'.B
  module E : ParserExpr with type texpr=B.type_expr and type benv=B.benv
  val parse_ty_ext : string -> B.ext
  val parse_expr_ext : string -> E.pexpr
end
