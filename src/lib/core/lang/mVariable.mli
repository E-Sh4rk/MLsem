open Common
open Types

type t = Variable.t

val create_let : bool (* mutable? *) -> string option -> t
val create_gen : bool (* mutable? *) -> string option -> t
val create_lambda : bool (* mutable? *) -> string option -> t
val is_mutable : Variable.t -> bool

val add_to_env : Env.t -> Variable.t -> Ty.t -> Env.t

val ref_cons : unit -> Ty.t
val ref_get : unit -> Ty.t
val ref_assign : unit -> Ty.t
