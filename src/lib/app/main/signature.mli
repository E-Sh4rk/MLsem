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

(* The (prefixed) names of [overload]'s free type variables, as rendered in its
   text. Per-overload: variables are not shared across overloads. *)
val overload_vars : overload -> string list

(* [instantiate overload name ty] substitutes every type variable of [overload]
   named [name] (as returned by [overload_vars]) with the concrete type [ty]. *)
val instantiate : overload -> string -> Ty.t -> overload

val pp_overload : Format.formatter -> overload -> unit
