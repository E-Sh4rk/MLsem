(** Gradual types.

    A gradual type is an {e interval} of static types: a lower bound [lb] (what is
    statically guaranteed) and an upper bound [ub] (what a runtime check might
    still allow), with [lb ≤ ub]. A static type is the degenerate interval
    [[t,t]], and [dyn] is the widest one, [[empty,any]]. Operations are applied
    pointwise to the bounds, so a computation on gradual types tracks in
    parallel the pessimistic and the optimistic reading of the program. *)

open Base
open Tvar

type t

val empty : t
val any : t

val dyn : t
(** The unknown type, i.e. the interval [[empty,any]]. *)

val mk: Ty.t -> t
(** The static (non-gradual) type [[t,t]]. *)

val mk_gradual: Ty.t -> Ty.t -> t
(** [mk_gradual lb ub] is the interval [[lb,ub]].
    @raise Invalid_argument if [lb] is not a subtype of [ub]. *)

val lb: t -> Ty.t
val ub: t -> Ty.t
val destruct : t -> Ty.t * Ty.t
val cup: t -> t -> t
val cap: t -> t -> t
val disj : t list -> t
val conj : t list -> t
val neg : t -> t
(** Negation swaps the bounds, as it is anti-monotonic. *)

val fv : t -> MVarSet.t
val substitute : Subst.t -> t -> t

(** {2 Lifting operations on static types}

    These lift an operation on {!Ty.t} to gradual types by applying it to both
    bounds; the operation must be {b monotonic} (anti-monotonic for {!map'}), so
    that the result is still a well-formed interval. *)

val map : (Ty.t -> Ty.t) -> t -> t
val map2 : (Ty.t -> Ty.t -> Ty.t) -> t -> t -> t
val mapl : (Ty.t list -> Ty.t) -> t list -> t

(** The [op] family additionally guards the operation with a domain check.
    The check is applied to the {b lower} bounds only: this is the optimistic
    reading required by gradual typing, under which the check always succeeds on
    [dyn] (whose lower bound is [empty]) and the responsibility is deferred to a
    runtime cast. [None] is returned when the check fails. *)

val op : (Ty.t -> bool) -> (Ty.t -> Ty.t) -> t -> t option
val op2 : (Ty.t -> Ty.t -> bool) -> (Ty.t -> Ty.t -> Ty.t) -> t -> t -> t option
val opl : (Ty.t list -> bool) -> (Ty.t list -> Ty.t) -> t list -> t option

val map' : (Ty.t -> Ty.t) -> t -> t
(** Like {!map}, for an {b anti-monotonic} operation (the bounds are swapped). *)

(** {2 Predicates}

    These are the {e definite} readings: they hold for every static type in the
    interval, not merely for some. *)

val is_empty : t -> bool
(** Whether the whole interval is empty, i.e. whether [ub] is. *)

val is_any : t -> bool
(** Whether the whole interval is [any], i.e. whether [lb] is. *)

val leq : t -> t -> bool
(** Bound-wise inclusion: both [lb] and [ub] must be smaller. This is stricter
    than consistent subtyping, which only requires the intervals to overlap. *)

val equiv : t -> t -> bool

val non_gradual : t -> bool
(** Whether the interval is a single static type, i.e. [lb] and [ub] are
    equivalent. *)

val simplify : t -> t
val factorize : t -> t

val pp : Format.formatter -> t -> unit
val pp' : Subst.t -> Format.formatter -> t -> unit
(** [pp' s] prints the type after applying the substitution [s], which is how
    variables get short display names. *)

module Builder : sig
    val dyn : unit -> Ty.t
    (** [dyn ()] returns a fresh instance of the dyn type for the builder.
        Each occurrence of dyn in a type should use a fresh instance. *)

    val refresh : Ty.t -> Ty.t
    (** [refresh ty] refreshes the instances of dyn in [ty].
        This function should be called when combining multiple occurrences
        of a gradual type that is in the process of being built. *)

    val non_gradual : Ty.t -> bool
    (** [non_gradual ty] returns true if and only if [ty] is not gradual,
        i.e. it has no instance of dyn. *)

    val is_valid : Ty.t -> bool
    (** [is_valid ty] returns true if and only if [ty] is a valid gradual type,
        i.e. it has no invariant instance of dyn. *)

    val build : Ty.t -> t
    (** [build ty] builds the gradual type corresponding to [ty].
        @raise Invalid_argument if a dyn occurs in an invariant position. *)

    val all_dyn_vars : unit -> TVarSet.t
    (** [all_dyn_vars ()] returns the current set of all dyn vars. *)

    val pp : Format.formatter -> Ty.t -> unit
    val pp' : Subst.t -> Format.formatter -> Ty.t -> unit
end
