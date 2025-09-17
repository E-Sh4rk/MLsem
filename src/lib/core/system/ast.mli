open Mlsem_common
open Mlsem_types

type pcustom = { pdom: Ty.t -> Ty.t ; proj: Ty.t -> Ty.t ; pgen: bool }
type ccustom = { cdom: Ty.t -> Ty.t list list ; cons: Ty.t list -> Ty.t ; cgen: bool }
type coerce = Check | CheckStatic | NoCheck
type projection =
| Pi of int * int | Field of string | Hd | Tl | PiTag of Tag.t
| PCustom of pcustom
type constructor =
| Tuple of int | Cons | Rec of (string * bool) list * bool | Tag of Tag.t | Enum of Enum.t 
| RecUpd of string | RecDel of string | Choice of int | Voidify
| CCustom of ccustom
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
| Conditional of t * Ty.t * t
and t = Eid.t * e

val map : (t -> t) -> t -> t
val map' : (t -> t option) -> t -> t
val iter : (t -> unit) -> t -> unit
val iter' : (t -> bool (* continue inside *)) -> t -> unit
val fv : t -> VarSet.t
val vars : t -> VarSet.t
val apply_subst : Subst.t -> t -> t

val coerce : coerce -> GTy.t -> t -> t

val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val pp_coerce : Format.formatter -> coerce -> unit
val pp_projection : Format.formatter -> projection -> unit
val pp_constructor : Format.formatter -> constructor -> unit
val pp_pcustom : Format.formatter -> pcustom -> unit
val pp_ccustom : Format.formatter -> ccustom -> unit
