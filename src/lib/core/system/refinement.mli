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

  val get : t -> Eid.t -> GREnv.t
  (** The refinement holding at a program point; the empty (trivially true)
      environment if none was recorded. *)

  val get_anonymous : t -> GREnv.t list
  val all : t -> GREnv.t list
  (** Every recorded refinement, located or not. *)

  val add : t -> Eid.t -> GREnv.t -> t
  (** Records a refinement at a program point, {b intersecting} it with any
      refinement already recorded there. Distinct program points must therefore
      carry distinct [Eid.t]s, or unrelated refinements get conflated. *)

  val add_anonymous : t -> GREnv.t -> t
  val map : (GREnv.t -> GREnv.t) -> t -> t
end

val typeof_def : Env.t -> Ast.t -> TyScheme.t
(** [typeof_def env e] returns an approximation of the type of the definition [e]
    under the environment [env], {b assuming} that [e] is typeable (this is not
    checked). For complex expressions whose type cannot be approximated without
    annotations, such as applications, it returns [any]. *)

val sufficient_refinements : safe:bool -> Env.t -> Ast.t -> Ty.t -> REnv.t list
(** [sufficient_refinements ~safe env e t] returns refinements under each of which
    (combined with [env]) [e] has type [t]: the upper bound (if [safe]) or the lower
    bound (otherwise) of the type of [e] is a subtype of [t]. The typeability of [e]
    is {b assumed}, not checked. Its result is a {b sufficient} condition, and may be
    empty when none can be found. *)

val refine : Env.t -> Ast.t -> GTy.t -> GREnv.t
(** [refine env e t] returns what must hold of the variables in scope for [e] to
    have type [t]: a {b necessary} condition for both for the static and dynamic components.
    It is always sound to under-refine, and the analysis does so whenever it cannot
    invert a construct, so the result may be the empty (trivially true) environment. *)

val refinements :
  ?extra_checks:(Eid.t * GTy.t) list ->
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
