open Parsing
open Variable
open Types.Base
open Env

type e =
| Abstract of typ
| Const of Ast.const
| Var of Variable.t
| Atom of atom
| Tag of tag * t
| Lambda of typ * Variable.t * t
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

val initial_env : Env.t
val is_fixpoint : t -> bool
val map : (t -> t) -> t -> t
val fold : (t -> 'a list -> 'a) -> t -> 'a
val fv : t -> VarSet.t
val from_parser_ast : Ast.expr -> t
val push_coercions : t -> t
val add_coercion : t -> typ -> t

val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val show : t -> string
val show_e : e -> string
