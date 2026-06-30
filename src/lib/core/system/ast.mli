open Mlsem_common
open Mlsem_types

type pcustom = { pname: string ; pdom: Ty.t -> Ty.t ; proj: Ty.t -> Ty.t ; pgen: bool }
type ccustom = { cname: string ; cdom: Ty.t -> Ty.t list list ; cons: Ty.t list -> Ty.t ; cgen: bool }
type ocustom = { oname: string ; ofun: TyScheme.t ; ogen: bool }
type check = Check | CheckStatic | NoCheck
type projection = (* Projections must be monotonic operations *)
| Pi of int * int | PiField of string | PiFieldOpt of string
| Hd | Tl | PiTag of Tag.t | PCustom of pcustom
type constructor = (* Constructors must be monotonic operations *)
| Tuple of int | Cons | Rec of string list * bool | Tag of Tag.t | Enum of Enum.t 
| Join of int | Meet of int | Ternary of Ty.t (* Should not contain type vars *)
| Voidify of Ty.t (* Should not contain type vars *)
| Normalize | CCustom of ccustom
type operation =
| RecUpd of string | RecDel of string
| OCustom of ocustom
type param_annot = GTy.t option
type e =
| Value of GTy.t
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of param_annot * Variable.t * t
| LambdaRec of (param_annot * Variable.t * t) list
| Ite of t * GTy.t * t * t
| App of t * t
| Operation of operation * t
| Projection of projection * t
| Let of (Ty.t list) (* empty list = no partitioning *) * Variable.t * t * t
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

val pp_raw : Format.formatter -> t -> unit
val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val pp_check : Format.formatter -> check -> unit
val pp_projection : Format.formatter -> projection -> unit
val pp_constructor : Format.formatter -> constructor -> unit
val pp_operation : Format.formatter -> operation -> unit
val pp_param_annot : Format.formatter -> param_annot -> unit
val pp_pcustom : Format.formatter -> pcustom -> unit
val pp_ccustom : Format.formatter -> ccustom -> unit


val domain_of_proj : projection -> Ty.t -> Ty.t
val proj : projection -> Ty.t -> Ty.t

val domains_of_construct : constructor -> Ty.t -> Ty.t list list
val construct : constructor -> Ty.t list -> Ty.t

val fun_of_operation : operation -> TyScheme.t

val coerce : ?coercion_id:Eid.t -> check -> GTy.t -> t -> t
val push_coercions : t -> t
