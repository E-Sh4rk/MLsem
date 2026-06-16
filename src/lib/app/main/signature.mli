open Mlsem_types

type overload
type t = overload list

val simplify_tyscheme : TyScheme.t -> TyScheme.t
val simplify_overload : overload -> overload

(* May raise [Invalid_argument] if the type is not fully resolved. *)
val build : Builder.benv -> TyExpr.t -> overload * Builder.benv
(* May raise [Invalid_argument] if the type is not fully resolved. *)
val of_tyscheme : TyScheme.t -> t

val decompose : ?recursive:bool -> overload -> t
val regroup : t -> overload
val merge : ?recursive:bool -> overload -> overload

val to_tyscheme : t -> TyScheme.t
val to_gty : overload -> GTy.t

val pp_overload : Format.formatter -> overload -> unit
