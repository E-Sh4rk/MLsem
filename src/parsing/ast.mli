open Types.Base
open Types.Additions
open Variable
open Pomap

exception SymbolError of string
exception LexicalError of Position.t * string
exception SyntaxError of Position.t * string

type varname = string
type exprid = int

type annotation = exprid Position.located

type const =
| Unit | Nil
| EmptyRecord
| Bool of bool
| Int of Z.t
| Float of float
| Char of char
| String of string

type projection = Pi of int * int | Field of string | Hd | Tl | PiTag of tag

type 'typ type_annot = Unnanoted | ADomain of 'typ list

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

and ('a, 'typ, 'ato, 'tag, 'v) ast =
| Abstract of 'typ
| Const of const
| Var of 'v
| Atom of 'ato
| Tag of 'tag * ('a, 'typ, 'ato, 'tag, 'v) t
| Lambda of ('typ type_annot) * 'v * ('a, 'typ, 'ato, 'tag, 'v) t
| Fixpoint of ('a, 'typ, 'ato, 'tag, 'v) t
| Ite of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ * ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| App of ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Let of 'v * ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Tuple of ('a, 'typ, 'ato, 'tag, 'v) t list
| Cons of ('a, 'typ, 'ato, 'tag, 'v) t * ('a, 'typ, 'ato, 'tag, 'v) t
| Projection of projection * ('a, 'typ, 'ato, 'tag, 'v) t
| RecordUpdate of ('a, 'typ, 'ato, 'tag, 'v) t * string * ('a, 'typ, 'ato, 'tag, 'v) t option
| TypeConstr of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ list
| TypeCoercion of ('a, 'typ, 'ato, 'tag, 'v) t * 'typ list
| PatMatch of ('a, 'typ, 'ato, 'tag, 'v) t * (('a, 'typ, 'tag, 'v) pattern * ('a, 'typ, 'ato, 'tag, 'v) t) list
| TopLevel of ('a, 'typ, 'ato, 'tag, 'v) t

and ('a, 'typ, 'ato, 'tag, 'v) t = 'a * ('a, 'typ, 'ato, 'tag, 'v) ast

type annot_expr = (annotation, typ, atom, tag, Variable.t) t
type expr = (unit, typ, atom, tag, Variable.t) t
type parser_expr = (annotation, type_expr, string, string, varname) t

module Expr : Pomap_intf.PARTIAL_ORDER with type el = expr
module ExprMap : Pomap_intf.POMAP with type key = expr

type name_var_map = Variable.t StrMap.t
val empty_name_var_map : name_var_map

val unique_exprid : unit -> exprid
val identifier_of_expr : (annotation, 'a, 'b, 'c, 'd) t -> exprid
val position_of_expr : (annotation, 'a, 'b, 'c, 'd) t -> Position.t

val new_annot : Position.t -> annotation
val copy_annot : annotation -> annotation

val dummy_pat_var : Variable.t

val parser_expr_to_annot_expr : type_env -> var_type_env -> name_var_map -> parser_expr -> annot_expr

(*val unannot : annot_expr -> expr*)
val unannot_and_normalize : annot_expr -> expr
(*val fv : annot_expr -> VarSet.t*)
val map_ast : (annot_expr -> annot_expr) -> annot_expr -> annot_expr 
val substitute : annot_expr -> Variable.t -> annot_expr -> annot_expr

val const_to_typ : const -> typ

type parser_element =
| Definition of (int (* log level *) * (string * parser_expr * type_expr option))
| Types of (string * string list * type_expr) list
| AbsType of string * variance list

type parser_program = (annotation * parser_element) list

(* Pretty printers *)

val pp_const : Format.formatter -> const -> unit
val pp_projection : Format.formatter -> projection -> unit
val pp_type_annot : (Format.formatter -> 'a -> unit) ->
                    Format.formatter -> 'a type_annot -> unit
val show_const : const -> string
val show_projection : projection -> string
val show_type_annot : (Format.formatter -> 'a -> unit) ->
                    'a type_annot -> string
