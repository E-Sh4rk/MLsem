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
val empty_name_var_map : name_var_map

val new_annot : Position.t -> annotation

val dummy_pat_var : Variable.t

val parser_expr_to_expr : type_env -> var_type_env -> name_var_map -> parser_expr -> expr

type parser_element =
| Definitions of (string * parser_expr) list
| SigDef of string * type_expr option
| Types of (string * string list * type_expr) list
| AbsType of string * Abstract.variance list
| Command of string * const

type parser_program = (annotation * parser_element) list
