open Mlsem_common
open Mlsem_types
module SA = Mlsem_system.Ast

type e =
| Hole of int
| Exc (** Expression of type [empty] *) | Void (** Expression of type [void] *)
| Voidify of t
(** Evaluate the expression inside, but give it the [void] type, even if it diverges *)
| Value of GTy.t
| Var of Variable.t
| Constructor of SA.constructor * t list
| Lambda of Ty.t list * GTy.t * Variable.t * t
(** The first parameter is a suggested type decomposition, similarly to let-bindings *)
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * GTy.t * t * t
| App of t * t
| Operation of SA.operation * t
| Projection of SA.projection * t
| Declare of Variable.t * t
(** Declaration of an uninitialized mutable variable.
    The variable provided must be mutable (cf. {!MVariable.create}).
    The type system will not check that the variable is assigned before use. *)
| Let of Ty.t list * Variable.t * t * t
(** The first parameter is a suggested type decomposition *)
| TypeCast of t * GTy.t * SA.check
| TypeCoerce of t * GTy.t * SA.check
| VarAssign of Variable.t * t
(** The variable provided must be mutable (cf. {!MVariable.create}) *)
| Loop of t
(** An expression that may be evaluated multiple times. Used to encode While expressions. *)
| Seq of t * t
(** Evaluate the first expression, then the second. *)
| Try of t * t
(** May jump from a branch to another. Used to model try-with expressions. *)
| Alt of t * t
(** Evaluate both branches independently. The result is (the intersection of) the result of the branches that do not fail. *)
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
