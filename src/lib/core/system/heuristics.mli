(** Default implementations for the normalization hooks of
    {!Mlsem_system.Config}. Both encode the same assumption about abstract
    types: an abstract type with an empty parameter is uninhabited (there is no
    value of type [ref(empty)], say). This is {e not} a neutral simplification —
    it is only sound for abstract types whose parameters are genuinely
    inhabited-by-construction. *)

open Mlsem_types

val normalize_empty_abstracts : Ty.t -> Ty.t
(** Removes from [ty] the abstract-type atoms that have an empty parameter:
    positive occurrences make their whole conjunction empty, negative ones are
    dropped as vacuous. Suitable as {!Mlsem_system.Config.normalization_fun}. *)

type tally_context = { mono: MVarSet.t ; tvars: MVarSet.t ; res: Ty.t }
(** Context of the tallying call whose solutions are being normalized: the
    monomorphic variables, the variables of the environment, and the type whose
    instance is being computed. *)

val normalize_abstract_factors : tally_context -> Subst.t list -> Subst.t list
(** Splits each solution that assigns to an environment variable a type
    containing several abstract-type atoms into one solution per atom, plus one
    for the remainder. This turns an approximate solution such as
    [α ↦ ref(int) & array(int)] into the alternatives the reconstruction can
    actually exploit. Suitable as
    {!Mlsem_system.Config.subst_normalization_fun}.
    Has no effect when the parameters are fully annotated. *)
