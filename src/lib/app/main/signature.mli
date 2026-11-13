open Mlsem_types

type overload
type t = overload list

val simplify_tyscheme : TyScheme.t -> TyScheme.t
val simplify_overload : overload -> overload

val build : Builder.benv -> TyExpr.t -> overload * Builder.benv
(* Whether the signature has holes (weak type variables): it is then only
   added to the environment once its definition is typed. *)
val is_draft : overload -> bool
(* May raise [Invalid_argument] if the type is not fully resolved. *)
val of_tyscheme : TyScheme.t -> t

val decompose : ?recursive:bool -> overload -> t
val regroup : t -> overload
val merge : ?recursive:bool -> overload -> overload

val to_tyscheme : t -> TyScheme.t
val to_gty : overload -> GTy.t

(* The (prefixed) names of [overload]'s free type variables, as rendered in its
   text. Per-overload: variables are not shared across overloads. *)
val overload_vars : overload -> string list

(* [instantiate overload name ty] substitutes every type variable of [overload]
   named [name] (as returned by [overload_vars]) with the concrete type [ty]. *)
val instantiate : overload -> string -> Ty.t -> overload

val pp_overload : Format.formatter -> overload -> unit
