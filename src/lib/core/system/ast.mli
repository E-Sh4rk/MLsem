open Common
open Types

type cf = CfWhile | CfCond | CfOther
type coerce = Check | CheckStatic | NoCheck
type projection =
| Pi of int * int | Field of string | Hd | Tl | PiTag of Tag.t
| PCustom of { pdom: Ty.t -> Ty.t ; proj: Ty.t -> Ty.t }
type constructor =
| Tuple of int | Cons | RecUpd of string | RecDel of string
| Tag of Tag.t | Enum of Enum.t | Choice of int
| CCustom of { cdom: Ty.t -> Ty.t list list ; cons: Ty.t list -> Ty.t }
type e =
| Value of GTy.t
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * Ty.t * t * t
| App of t * t
| Projection of projection * t
| Let of (Ty.t list) * Variable.t * t * t
| TypeCast of t * Ty.t
| TypeCoerce of t * GTy.t * coerce
| ControlFlow of cf * t * Ty.t * t option * t option
and t = Eid.t * e

val map : (t -> t) -> t -> t
val fold : (t -> 'a list -> 'a) -> t -> 'a
val fv : t -> VarSet.t
val apply_subst : Subst.t -> t -> t
val substitute : Variable.t -> Variable.t -> t -> t
val coerce : coerce -> GTy.t -> t -> t

val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val pp_cf : Format.formatter -> cf -> unit
val pp_coerce : Format.formatter -> coerce -> unit
val pp_projection : Format.formatter -> projection -> unit
val pp_constructor : Format.formatter -> constructor -> unit
