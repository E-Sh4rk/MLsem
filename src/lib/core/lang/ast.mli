open Common
open Types
module SA = System.Ast

type pattern_constructor =
| PCTuple of int
| PCCons
| PCRec of string list * bool
| PCTag of Tag.t
| PCEnum of Enum.t
| PCCustom of SA.ccustom * SA.pcustom list
[@@deriving show]
type pattern =
| PType of Ty.t
| PVar of Variable.t
| PConstructor of pattern_constructor * pattern list
| PAnd of pattern * pattern
| POr of pattern * pattern
| PAssign of Variable.t * GTy.t
[@@deriving show]
type e =
| Void
| Value of GTy.t
| Var of Variable.t
| Constructor of SA.constructor * t list
| Lambda of Ty.t list (* Decomposition, similar to Let bindings *) * GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * Ty.t * t * t
| PatMatch of t * (pattern * t) list
| App of t * t
| Projection of SA.projection * t
| Let of Ty.t list * Variable.t * t * t
| TypeCast of t * Ty.t
| TypeCoerce of t * GTy.t * SA.coerce
| VarAssign of Variable.t * t (* Will be untypeable if v is not mutable *)
| Conditional of bool (* allow break *) * t * Ty.t * t * t (* Conditional void blocks *)
| If of t * Ty.t * t * t option
| While of t * Ty.t * t
| Seq of t * t
| Return of t option
| Break
| Hole of int
[@@deriving show]
and t = Eid.t * e
[@@deriving show]

val map_pat : (pattern -> pattern) -> pattern -> pattern
val map_pat' : (pattern -> pattern option) -> pattern -> pattern
val iter_pat : (pattern -> unit) -> pattern -> unit
val iter_pat' : (pattern -> bool (* continue inside *)) -> pattern -> unit
val fill_hole : int -> t -> t -> t

val map : (t -> t) -> t -> t
val map' : (t -> t option) -> t -> t
val iter : (t -> unit) -> t -> unit
val iter' : (t -> bool (* continue inside *)) -> t -> unit
val fv : t -> VarSet.t
val vars : t -> VarSet.t
val rename_fv : Variable.t -> Variable.t -> t -> t

val pp_pattern_constructor : Format.formatter -> pattern_constructor -> unit
val pp_pattern : Format.formatter -> pattern -> unit
val pp_e : Format.formatter -> e -> unit
val pp : Format.formatter -> t -> unit
