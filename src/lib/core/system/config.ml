(** Tuning knobs of the type system.

    These are global mutable settings, read at each use rather than captured, so
    changing one affects every subsequent type-checking operation. *)

open Mlsem_types

(** Whether generalization is restricted to expressions whose evaluation cannot
    have an effect (cf. [Checker.generalize]). Disabling it makes inference
    more permissive but unsound in the presence of mutable state. *)
let value_restriction = ref true

(** Whether the reconstruction should try to infer intersection (overloaded)
    types for functions, by exploring the branches of a typecase separately even
    when the scrutinee's type does not decide between them. Disabling it makes
    inference cheaper and the inferred types simpler. *)
let infer_overload = ref true

(** Whether a branch of an intersection whose exploration failed should be
    explored again in the other branches' domains. Disabling it makes
    reconstruction cheaper at the cost of precision. *)
let reexplore_failed_domains = ref true

(** Normalization applied to a type before its emptiness is tested. It must
    return a type that is empty if and only if the argument is "empty enough" to
    be considered uninhabited — which is where assumptions about abstract types
    are injected (cf. [Heuristics.normalize_empty_abstracts]). Use
    [Fun.id] for no normalization. *)
let normalization_fun : (Ty.t -> Ty.t) ref = ref Heuristics.normalize_empty_abstracts

(** Normalization applied to the set of solutions of a tallying instance. It may
    split, reorder or drop solutions, but every returned substitution must be a
    solution of the instance: the reconstruction relies on them being sound, not
    on them being complete (cf. [Heuristics.normalize_abstract_factors]).
    Use [fun _ x -> x] for no normalization. *)
let subst_normalization_fun : (Heuristics.tally_context -> Subst.t list -> Subst.t list) ref =
  ref Heuristics.normalize_abstract_factors
