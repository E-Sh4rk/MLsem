open Parsing
open Variable
open Types.Base

type e =
| Abstract of typ
| Const of Ast.const
| Var of Variable.t
| Atom of atom
| Tag of tag * t
| Lambda of typ * Variable.t * t
| LambdaRec of (typ * Variable.t * t) list
| Ite of t * typ * t * t
| App of t * t
| Tuple of t list
| Cons of t * t
| Projection of Ast.projection * t
| RecordUpdate of t * string * t option
| Let of (typ list) * Variable.t * t * t
| TypeConstr of t * typ
| TypeCoerce of t * typ
and t = Ast.exprid * e

val map : (t -> t) -> t -> t
val fold : (t -> 'a list -> 'a) -> t -> 'a
val fv : t -> VarSet.t
val from_parser_ast : Ast.expr -> t
val coerce : typ -> t -> t

val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val show : t -> string
val show_e : e -> string
