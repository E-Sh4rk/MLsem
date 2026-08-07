open Var
open Mlsem_types

module type Env = sig
    type t
    type ty

    val empty : t
    val is_empty : t -> bool
    (** Whether the environment has no binding at all. *)

    val singleton : Variable.t -> ty -> t
    val construct : (Variable.t * ty) list -> t

    val add : Variable.t -> ty -> t -> t
    (** Binds a variable that is {b not} already bound; use {!replace}
        otherwise.
        @raise Invalid_argument if the variable is already bound. *)

    val replace : Variable.t -> ty -> t -> t
    (** Binds a variable, overriding any previous binding. *)

    val domain : t -> Variable.t list
    val bindings : t -> (Variable.t * ty) list
    val mem : Variable.t -> t -> bool

    val find : Variable.t -> t -> ty
    (** @raise Not_found if the variable is not bound. *)

    val find_opt : Variable.t -> t -> ty option
    val rm : Variable.t -> t -> t
    val rms : Variable.t list -> t -> t
    val restrict : Variable.t list -> t -> t
    val map : (ty -> ty) -> t -> t
    val filter : (Variable.t -> ty -> bool) -> t -> t

    val tvars : t -> MVarSet.t
    (** An {b over-approximation} of the type variables occurring in the
        environment: it is maintained incrementally and not recomputed when a
        binding is combined with another. Sound wherever it is used to build a
        set of monomorphic variables, but not a reliable "occurs" test. *)

    val substitute : Subst.t -> t -> t

    val equiv : t -> t -> bool

    val leq : t -> t -> bool
    (** [leq env1 env2] holds when [env1] is at least as precise as [env2]:
        it binds at least the same variables, at smaller types. *)

    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val pp_filtered : string list -> Format.formatter -> t -> unit
    (** Like {!pp}, restricted to the variables whose display name is listed. *)
end

(** Typing environment: binds each variable in scope to a type scheme.

    @canonical Mlsem_common.Env *)
module Env : Env with type ty:=TyScheme.t

(** Refinement environment: a conjunction of constraints "variable [v] has type
    [t]", used to narrow a typing environment at a given program point.

    Unlike {!Env}, an [REnv] is {b partial}: a variable it does not bind is
    unconstrained, i.e. implicitly bound to [any] (see {!REnv.find'}). So the
    empty environment is the trivially true constraint, and an environment
    binding some variable to [empty] is unsatisfiable. This convention explains
    the approximations below.

    @canonical Mlsem_common.REnv *)
module REnv : sig
  include Env with type ty:=Ty.t

  val find' : Variable.t -> t -> Ty.t
  (** Like {!find}, returning [any] instead of raising [Not_found] for an
      unbound variable. This is the reading that matches the partiality of an
      [REnv]. *)

  val cap : t -> t -> t
  (** Conjunction of two refinements. *)

  val conj : t list -> t

  val neg : t -> t list
  (** Negation, as a {b disjunction}: negating a conjunction of constraints
      yields one alternative per constraint. The empty environment (true)
      negates to the empty list (false). *)

  val cup_approx : t -> t -> t
  (** Over-approximation of the disjunction of two refinements: a variable
      constrained in only one of them becomes unconstrained, since nothing can
      be assumed about it in the other branch. *)

  val disj_approx : t list -> t
  (** @raise Invalid_argument on the empty list, which has no
      representation. *)

  val neg_approx : t -> t option
  (** {!neg} followed by {!disj_approx}; [None] represents the unsatisfiable
      refinement, which {!disj_approx} cannot express. *)

  val refine_env : Env.t -> t -> Env.t
  (** [refine_env env renv] intersects the type of each variable of [env] with
      its constraint in [renv]. Variables of [renv] that are not in [env] are
      ignored: refining never extends the scope.
      @raise Invalid_argument if a refined variable has a scheme quantifying a
      variable that occurs in the constraint. *)
end
