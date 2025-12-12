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

module ParserExpr(B:Builder'.B) : ParserExpr with type texpr=B.type_expr and type benv=B.benv

module type ParserExt = sig
  module B : Mlsem_types.Builder'.B
  module E : ParserExpr with type texpr=B.type_expr and type benv=B.benv
  val parse_ty_ext : string -> B.ext
  val parse_expr_ext : Position.t -> string -> E.pexpr
end
