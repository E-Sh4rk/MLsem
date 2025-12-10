open Mlsem_common
open Mlsem_types

type pcustom = { pname: string ; pdom: Ty.t -> Ty.t ; proj: Ty.t -> Ty.t ; pgen: bool }
type ccustom = { cname: string ; cdom: Ty.t -> Ty.t list list ; cons: Ty.t list -> Ty.t ; cgen: bool }
type ocustom = { oname: string ; ofun: TyScheme.t ; ogen: bool }
type check = Check | CheckStatic | NoCheck
type projection =
| Pi of int * int | PiField of string | PiFieldOpt of string
| Hd | Tl | PiTag of Tag.t | PCustom of pcustom
type constructor =
| Tuple of int | Cons | Rec of string list * bool | Tag of Tag.t | Enum of Enum.t 
| Join of int | Meet of int | Negate | Ternary of Ty.t (* Should not contain type vars *)
| CCustom of ccustom
type operation =
| RecUpd of string | RecDel of string
| OCustom of ocustom
type e =
| Value of GTy.t
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * GTy.t * t * t
| App of t * t
| Operation of operation * t
| Projection of projection * t
| Let of (Ty.t list) * Variable.t * t * t
| TypeCast of t * GTy.t * check
| TypeCoerce of t * GTy.t * check
| Alt of t * t
and t = Eid.t * e

val map : (t -> t) -> t -> t
val map' : (t -> t option) -> t -> t
val iter : (t -> unit) -> t -> unit
val iter' : (t -> bool (* continue inside *)) -> t -> unit
val fv : t -> VarSet.t
val vars : t -> VarSet.t
val apply_subst : Subst.t -> t -> t

val coerce : check -> GTy.t -> t -> t

val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val pp_check : Format.formatter -> check -> unit
val pp_projection : Format.formatter -> projection -> unit
val pp_constructor : Format.formatter -> constructor -> unit
val pp_operation : Format.formatter -> operation -> unit
val pp_pcustom : Format.formatter -> pcustom -> unit
val pp_ccustom : Format.formatter -> ccustom -> unit
