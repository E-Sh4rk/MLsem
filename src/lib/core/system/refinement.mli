(** Occurrence typing: computing what a test tells us about the variables in
    scope.

    The results feed the reconstruction in two ways, which
    {!Mlsem_system.Reconstruction.initial} enables independently:
    - {e direct narrowing} attaches a refinement to a program point, so that the
      checker types that sub-expression under a narrowed environment;
    - {e partition narrowing} turns the collected refinements into suggested
      type decompositions for let-bound variables, so that a definition can be
      typed once per case. *)

open Mlsem_common
open Mlsem_types

(** Refinements collected over a whole expression: those attached to a program
    point, plus anonymous ones that only contribute to partitioning. *)
module Refinements : sig
  type t
  val empty : t

  val get : t -> Eid.t -> REnv.t
  (** The refinement holding at a program point; the empty (trivially true)
      environment if none was recorded. *)

  val get_anonymous : t -> REnv.t list
  val all : t -> REnv.t list
  (** Every recorded refinement, located or not. *)

  val add : t -> Eid.t -> REnv.t -> t
  (** Records a refinement at a program point, {b intersecting} it with any
      refinement already recorded there. Distinct program points must therefore
      carry distinct [Eid.t]s, or unrelated refinements get conflated. *)

  val add_anonymous : t -> REnv.t -> t
  val map : (REnv.t -> REnv.t) -> t -> t
end

val typeof_def : Env.t -> Ast.t -> TyScheme.t
(** [typeof_def env e] returns an approximation of the type of the definition [e]
    under the environment [env]. This approximation is [TyScheme.any] for non-trivial cases,
    but it can be more precise for simple constructs (e.g. variables, casts, projections)
    or in the presence of user type annotations (coercions). *)

val refine : Env.t -> Ast.t -> Ty.t -> REnv.t
(** [refine env e t] returns what must hold of the variables in scope for [e] to
    have type [t]: a {b necessary} condition, obtained as a fixpoint over the
    negation of the sufficient conditions for [e] to {e not} have type [t]. It
    is always sound to under-refine, and the analysis does so whenever it cannot
    invert a construct, so the result may be the empty (trivially true)
    environment. *)

val refinements :
  ?extra_checks:(Eid.t * Ty.t) list ->
  ?refine_on_typecases:bool ->
  ?refine_on_casts:bool ->
  Env.t -> Ast.t -> Refinements.t
(** Collects the refinements of a whole expression by calling {!refine} at each
    test. [extra_checks] adds "expression [eid] has type [t]" hypotheses to
    refine on, [refine_on_typecases] (default [true]) refines the branches of
    each typecase, and [refine_on_casts] (default [false]) also refines on type
    casts. *)

(** Turns collected refinements into type decompositions for let-bound
    variables. A decomposition splits a variable's type into cases that are
    typed separately, so that each branch of a later test sees a precise
    type. *)
module Partitioner : sig
  type t
  val from_refinements : Refinements.t -> t

  val filter_compatible : t -> Variable.t -> Ty.t -> t
  (** Keeps the refinements that are compatible with [v] having type [t], i.e.
      those to be considered inside the corresponding case. *)

  val decomposition_for : t -> Variable.t -> Ty.t list -> Ty.t list
  (** [decomposition_for t v suggs] refines the suggested decomposition [suggs]
      of [v] by splitting each of its cells along the refinements recorded for
      [v]. Returns the empty list — meaning "do not partition [v]" — exactly
      when [suggs] is empty. *)
end
