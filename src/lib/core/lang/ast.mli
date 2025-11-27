open Mlsem_common
open Mlsem_types
module SA = Mlsem_system.Ast

type blockid = BFun | BLoop | BOther of int
type pattern_constructor =
| PCTuple of int
| PCCons
| PCRec of string list * bool
| PCTag of Tag.t
| PCEnum of Enum.t
| PCCustom of SA.ccustom * SA.pcustom list
type pattern =
| PType of Ty.t
| PVar of Ty.t list * Variable.t
(** The first parameter is a suggested type decomposition, similarly to let-bindings *)
| PConstructor of pattern_constructor * pattern list
| PAnd of pattern * pattern
| POr of pattern * pattern
| PAssign of Ty.t list * Variable.t * GTy.t
(** The first parameter is a suggested type decomposition, similarly to let-bindings *)

type e =
| Hole of int
| Exc (** Expression of type [empty] *) | Void (** Expression of type [void] *)
| Voidify of t
(** Evaluate the expression inside, but give it the [void] type, even if it diverges *)
| Isolate of t
(** Prevent duplications of the outer context when eliminating control flow inside *)
| Value of GTy.t
| Var of Variable.t
| Constructor of SA.constructor * t list
| Lambda of Ty.t list * GTy.t * Variable.t * t
(** The first parameter is a suggested type decomposition, similarly to let-bindings *)
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * GTy.t * t * t
| PatMatch of t * (pattern * t) list
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
| Try of t * t
(** May jump from a branch to another. Used to model try-with expressions. *)
| Seq of t * t
(** Evaluate the first expression, then the second. *)
| Alt of t * t
(** Evaluate both branches independently. The result is (the intersection of) the result of the branches that do not fail. *)
| Block of blockid * t
(** Identifies a block to which a Ret can refer to. *)
| Ret of blockid * t option
(** Evaluate the expression specified ([Exc] if not specified), and use it as the value of block specified.
    Used to encode [Return] and [Break]. *)
| If of t * GTy.t * t * t option
(** If-else block statement. Returns void. *)
| While of t * GTy.t * t
(** While block statement. Returns void. *)
| Return of t
(** Exits the current function. *)
| Break
(** Exits the current while loop. *)
and t = Eid.t * e

val map_pat : (pattern -> pattern) -> pattern -> pattern
val map_pat' : (pattern -> pattern option) -> pattern -> pattern
val iter_pat : (pattern -> unit) -> pattern -> unit
val iter_pat' : (pattern -> bool (* continue inside *)) -> pattern -> unit

val map : (t -> t) -> t -> t
val map' : (t -> t option) -> t -> t
val iter : (t -> unit) -> t -> unit
val iter' : (t -> bool (* continue inside *)) -> t -> unit
val fill_hole : int -> t -> t -> t
val fv : t -> VarSet.t
val vars : t -> VarSet.t
val rename_fv : Variable.t -> Variable.t -> t -> t

val pp_blockid : Format.formatter -> blockid -> unit
val pp_pattern_constructor : Format.formatter -> pattern_constructor -> unit
val pp_pattern : Format.formatter -> pattern -> unit
val pp_e : Format.formatter -> e -> unit
val pp : Format.formatter -> t -> unit
