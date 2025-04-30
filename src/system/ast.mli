open Parsing
open Variable
open Types.Base

type e =
| Abstract of typ
| Const of Ast.const
| Var of Variable.t
| Atom of atom
| Tag of tag * e
| Lambda of (typ list) * Variable.t * e
| Ite of e * typ * e * e
| App of e * e
| Tuple of e list
| Cons of e * e
| Projection of Ast.projection * e
| RecordUpdate of e * string * e option
| Let of (typ list) * Variable.t * e * e
| TypeConstr of e * typ
| TypeCoerce of e * typ list

val initial_env : Env.t
val map : (e -> e) -> e -> e
val fold : (e -> 'a list -> 'a) -> e -> 'a
val fv : e -> VarSet.t
val from_parser_ast : Ast.annot_expr -> e
