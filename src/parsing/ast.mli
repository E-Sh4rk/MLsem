open Types.Base
open Types.Additions
open Variable
open System.Ast

exception SymbolError of string
exception LexicalError of Position.t * string
exception SyntaxError of Position.t * string

type varname = string

type annotation = exprid Position.located

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

and ('a, 'typ, 'ato, 'tag, 'v) ast =
| Abstract of 'typ
| Const of const
| Var of 'v
| Atom of 'ato
| Tag of 'tag * ('a, 'typ, 'ato, 'tag, 'v) t
| Suggest of 'v * 'typ list * ('a, 'typ, 'ato, 'tag, 'v) t
| Lambda of 'v * 'typ lambda_annot * ('a, 'typ, 'ato, 'tag, 'v) t
| LambdaRec of ('v * 'typ option * ('a, 'typ, 'ato, 'tag, 'v) t) list
| Ite of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ * ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| App of ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Let of 'v * ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Tuple of ('a, 'typ, 'ato, 'tag, 'v) t list
| Cons of ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Projection of projection * ('a, 'typ, 'ato, 'tag, 'v) t
| RecordUpdate of ('a, 'typ, 'ato, 'tag, 'v) t * string * ('a, 'typ, 'ato, 'tag, 'v) t option
| TypeConstr of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ
| TypeCoerce of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ
| PatMatch of ('a, 'typ, 'ato, 'tag, 'v) t * (('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'ato, 'tag, 'v) t) list
| Cond of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ * ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t option
| While of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ * ('a, 'typ, 'ato, 'tag, 'v) t
| Seq of ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t

and ('a, 'typ, 'ato, 'tag, 'v) t = 'a * ('a, 'typ, 'ato, 'tag, 'v) ast

type expr = (exprid, typ, atom, tag, Variable.t) t
type parser_expr = (annotation, type_expr, string, string, varname) t

type name_var_map = Variable.t StrMap.t
val empty_name_var_map : name_var_map

val new_annot : Position.t -> annotation
val copy_annot : annotation -> annotation

val dummy_pat_var : Variable.t

val parser_expr_to_expr : type_env -> var_type_env -> name_var_map -> parser_expr -> expr

type parser_element =
| Definitions of (string * parser_expr) list
| SigDef of string * type_expr
| Types of (string * string list * type_expr) list
| AbsType of string * variance list
| Command of string * const

type parser_program = (annotation * parser_element) list
