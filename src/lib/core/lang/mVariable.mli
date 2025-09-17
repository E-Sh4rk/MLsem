open Common
open Types

type t = Variable.t
type kind = Immut | AnnotMut of Ty.t | Mut

val create_let : kind -> string option -> t
val create_gen : kind -> string option -> t
val create_lambda : kind -> string option -> t
val is_mutable : Variable.t -> bool
val kind : Variable.t -> kind
val kind_equal : kind -> kind -> bool

val add_to_env : Env.t -> Variable.t -> Env.t

val ref_cons : Variable.t -> Ty.t
val ref_get : Variable.t -> Ty.t
val ref_assign : Variable.t -> Ty.t
