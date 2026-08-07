(** Type schemes: a gradual type together with the variables it quantifies
    universally. The other variables of the type are free, i.e. monomorphic. *)

open Tvar

type t

val mk : MVarSet.t -> GTy.t -> t
(** [mk vs ty] quantifies the variables of [vs] that actually occur in [ty]. *)

val mk_poly_except : MVarSet.t -> GTy.t -> t
(** [mk_poly_except mono ty] quantifies every variable of [ty] except those of
    [mono]. This is how a definition is generalized against its environment. *)

val mk_mono : GTy.t -> t
val mk_poly : GTy.t -> t

val get : t -> MVarSet.t * GTy.t
(** The quantified variables and the type, as they are stored. *)

val get_fresh : t -> MVarSet.t * GTy.t
(** An instance of the scheme in which the quantified variables have been
    renamed to fresh [KInfer] ones, together with the remaining
    (monomorphic) variables. This is how a scheme is used at an occurrence. *)

val fv : t -> MVarSet.t
(** The free, i.e. non-quantified, variables. *)

val substitute : Subst.t -> t -> t
(** Substitutes the free variables only; the quantified ones are left alone. *)

val leq : t -> t -> bool
(** A {b syntactic} approximation of scheme subsumption: it checks that the
    right-hand side does not quantify a variable that is free on the left, and
    then compares the two bodies with {!GTy.leq}. It does {e not} look for an
    instantiation, so it may answer [false] for schemes that are in fact
    related. *)

val equiv : t -> t -> bool

val bot_instance : t -> t
(** Replaces the quantified variables occurring only positively by [empty] and
    only negatively by [any], yielding the smallest instance of the scheme.
    Used to simplify a scheme without losing precision on the variables that
    occur in both polarities. *)

val top_instance : t -> t
(** Dual of {!bot_instance}: the largest instance. *)

val factorize : t -> t
val simplify : t -> t
val simplify_factorize : t -> t

val pp : Format.formatter -> t -> unit
val pp' : Subst.t -> Format.formatter -> t -> unit
val pp_short : Format.formatter -> t -> unit
(** Prints with the quantified variables renamed to short names ([a], [b], …). *)
