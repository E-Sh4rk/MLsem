open Base
open Tvar

type t
val empty : t
val any : t
val dyn : t
val mk: Ty.t -> t
val mk_gradual: Ty.t -> Ty.t -> t
val lb: t -> Ty.t
val ub: t -> Ty.t
val destruct : t -> Ty.t * Ty.t
val cup: t -> t -> t
val cap: t -> t -> t
val disj : t list -> t
val conj : t list -> t
val neg : t -> t

val fv : t -> MVarSet.t
val substitute : Subst.t -> t -> t
(* Mapping functions below assume the operation is monotonic *)
val map : (Ty.t -> Ty.t) -> t -> t
val map2 : (Ty.t -> Ty.t -> Ty.t) -> t -> t -> t
val mapl : (Ty.t list -> Ty.t) -> t list -> t
val op : (Ty.t -> bool) -> (Ty.t -> Ty.t) -> t -> t option
val op2 : (Ty.t -> Ty.t -> bool) -> (Ty.t -> Ty.t -> Ty.t) -> t -> t -> t option
val opl : (Ty.t list -> bool) -> (Ty.t list -> Ty.t) -> t list -> t option
(* Mapping functions below assume the operation is anti-monotonic *)
val map' : (Ty.t -> Ty.t) -> t -> t

val is_empty : t -> bool
val is_any : t -> bool
val leq : t -> t -> bool
val equiv : t -> t -> bool
val non_gradual : t -> bool

val simplify : t -> t
val normalize : t -> t

val pp : Format.formatter -> t -> unit
val pp' : Subst.t -> Format.formatter -> t -> unit

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
        Raises [Invalid_argument] if a dyn occurs in an invariant position. *)

    val all_dyn_vars : unit -> TVarSet.t
    (** [all_dyn_vars ()] returns the current set of all dyn vars. *)

    val pp : Format.formatter -> Ty.t -> unit
    val pp' : Subst.t -> Format.formatter -> Ty.t -> unit
end
