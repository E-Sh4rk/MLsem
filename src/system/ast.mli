open Parsing
open Variable
open Types.Base
open Types.Tvar

val typeof_const : Parsing.Ast.const -> typ

type cf = CfWhile | CfCond

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
| ControlFlow of cf * t * typ * t * t
and t = Ast.exprid * e

val map : (t -> t) -> t -> t
val fold : (t -> 'a list -> 'a) -> t -> 'a
val fv : t -> VarSet.t
val apply_subst : Subst.t -> t -> t
val from_parser_ast : Ast.expr -> t
val coerce : typ -> t -> t

val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val pp_cf : Format.formatter -> cf -> unit
val show : t -> string
val show_e : e -> string
val show_cf : cf -> string
