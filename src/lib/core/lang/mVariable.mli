open Common
open Types

type t = Variable.t

val create : string option -> t
val is_mutable : Variable.t -> bool

val add_to_env : Env.t -> Variable.t -> Ty.t -> Env.t

val ref_cons : unit -> Ty.t
val ref_get : unit -> Ty.t
val ref_assign : unit -> Ty.t
