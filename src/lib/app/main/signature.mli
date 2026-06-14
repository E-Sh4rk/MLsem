open Mlsem_types
open Mlsem_common

type overload
type t = overload list

val simplify : TyScheme.t -> TyScheme.t

(* May raise [Invalid_argument] if the type is not fully resolved. *)
val build : Builder.benv -> TyExpr.t -> t * Builder.benv
(* May raise [Invalid_argument] if the type is not fully resolved. *)
val of_tyscheme : TyScheme.t -> t

val to_tyscheme : Env.t -> t -> TyScheme.t
val to_gty : overload -> GTy.t

val merge : t -> overload

val pp_overload : Format.formatter -> overload -> unit
