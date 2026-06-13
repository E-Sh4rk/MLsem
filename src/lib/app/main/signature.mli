open Mlsem_types
open Mlsem_common

type overload = GTy.t
type t = overload list

val is_well_formed : overload -> bool
val simplify : TyScheme.t -> TyScheme.t
val to_tyscheme : Env.t -> t -> TyScheme.t
val of_tyscheme : TyScheme.t -> t
val merge : t -> overload
