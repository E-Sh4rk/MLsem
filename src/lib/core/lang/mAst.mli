open Mlsem_common
open Mlsem_types
module SA = Mlsem_system.Ast

type e =
| Hole of int
| Exc | Void | Voidify of t
| Value of GTy.t
| Var of Variable.t
| Constructor of SA.constructor * t list
| Lambda of Ty.t list (* Decomposition, similar to Let bindings *) * GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * Ty.t * t * t
| App of t * t
| Projection of SA.projection * t
| Declare of Variable.t * t (* Cannot be translated to system AST if v is not mutable *)
| Let of Ty.t list * Variable.t * t * t
| TypeCast of t * Ty.t
| TypeCoerce of t * GTy.t * SA.coerce
| VarAssign of Variable.t * t (* Cannot be translated to system AST if v is not mutable *)
| Seq of t * t
| Try of t * t
and t = Eid.t * e

val map : (t -> t) -> t -> t
val map' : (t -> t option) -> t -> t
val iter : (t -> unit) -> t -> unit
val iter' : (t -> bool (* continue inside *)) -> t -> unit
val fill_hole : int -> t -> t -> t
val fv : t -> VarSet.t
val vars : t -> VarSet.t
val rename_fv : Variable.t -> Variable.t -> t -> t

val to_system_ast : t -> SA.t

val pp_e : Format.formatter -> e -> unit
val pp : Format.formatter -> t -> unit
