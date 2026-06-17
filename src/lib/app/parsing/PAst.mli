open Mlsem_common
open Mlsem_types
open Mlsem_system.Ast
open Mlsem_lang

exception SymbolError of string
exception LexicalError of Position.t * string
exception SyntaxError of Position.t * string

(** Expr AST *)

type 'typ lambda_annot = 'typ option
type 'typ vkind = Immut | AnnotMut of 'typ | Mut
type ('typ,'v) vdef = 'typ vkind * 'v

type ('a, 'typ, 'gty, 'tag, 'v) pattern =
| PatType of 'typ
| PatVar of ('gty,'v) vdef
| PatLit of Const.t
| PatTag of 'tag * ('a, 'typ, 'gty, 'tag, 'v) pattern
| PatAnd of ('a, 'typ, 'gty, 'tag, 'v) pattern * ('a, 'typ, 'gty, 'tag, 'v) pattern
| PatOr of ('a, 'typ, 'gty, 'tag, 'v) pattern * ('a, 'typ, 'gty, 'tag, 'v) pattern
| PatTuple of ('a, 'typ, 'gty, 'tag, 'v) pattern list
| PatCons of ('a, 'typ, 'gty, 'tag, 'v) pattern * ('a, 'typ, 'gty, 'tag, 'v) pattern
| PatRecord of (string * (('a, 'typ, 'gty, 'tag, 'v) pattern)) list * bool
| PatAssign of ('gty,'v) vdef * Const.t

and ('a, 'typ, 'gty, 'enu, 'tag, 'v) ast =
| Magic of 'gty
| Const of Const.t
| Var of 'v
| Enum of 'enu
| Tag of 'tag * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| TagProj of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * 'tag
| Suggest of 'v * 'typ list * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Lambda of 'v * 'gty lambda_annot * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| LambdaRec of ('v * 'gty lambda_annot * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t) list
| Ite of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * 'gty * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| App of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Let of ('gty,'v) vdef * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Declare of ('gty,'v) vdef * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Tuple of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t list
| TupleProj of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * int * int
| Cons of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Hd of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t | Tl of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Record of (string * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t) list
| RecordUpdate of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * string * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t option
| RecordProj of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * string
| TypeCast of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * 'gty * check
| TypeCoerce of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * 'gty * check
| VarAssign of 'v * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| PatMatch of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * (('a, 'typ, 'gty, 'tag, 'v) pattern * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t) list
| Cond of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * 'gty * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t option
| While of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * 'gty * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Seq of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Alt of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t * ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Return of ('a, 'typ, 'gty, 'enu, 'tag, 'v) t
| Break | Continue

and ('a, 'typ, 'gty, 'enu, 'tag, 'v) t = 'a * ('a, 'typ, 'gty, 'enu, 'tag, 'v) ast

type expr = (Eid.t, Ty.t, GTy.t, Enum.t, Tag.t, Variable.t) t

(** Program AST *)

type varname = string
type annotation = Eid.t Position.located
val new_annot : Position.t -> annotation

type pexpr = (annotation, TyExpr.t, TyExpr.t, string, string, varname) t
type pat = (annotation, TyExpr.t, TyExpr.t, string, varname) pattern

module NameMap : Map.S with type key=string
type name_var_map = Variable.t NameMap.t
val empty_name_var_map : name_var_map

val to_expr : Builder.benv -> name_var_map -> pexpr -> expr * Builder.benv

type element =
| Definitions of (Position.t * (TyExpr.t, string) vdef * pexpr) list
| SigDef of string * bool (* mutable *) * TyExpr.t
| Types of (string * string list * TyExpr.t) list
| AbsType of string * int
| Command of string * Const.t

type program = (annotation * element) list
