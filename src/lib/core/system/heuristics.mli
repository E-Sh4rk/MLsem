(** Heuristics used by the type inference (cf. {!Mlsem_system.Config}). *)

open Mlsem_types

val normalize_empty_abstracts : Ty.t -> Ty.t
(** Removes from [ty] the abstract-type atoms that have an empty parameter:
    positive occurrences make their whole conjunction empty, negative ones are
    dropped as vacuous. This encodes the assumption that an abstract type with an
    empty parameter is uninhabited (there is no value of type [ref(empty)], say),
    which is {e not} a neutral simplification: it is only sound for abstract types
    whose parameters are genuinely inhabited-by-construction.
    Suitable as {!Mlsem_system.Config.normalization_fun}. *)

type tally_context = { mono: MVarSet.t ; tvars: MVarSet.t ; res: Ty.t }
(** Context of the tallying call whose solutions are being normalized: the
    monomorphic variables, the variables of the environment, and the type whose
    instance is being computed. *)

val factorize_abstracts : MVarSet.t -> GTy.t -> Subst.t
(** [factorize_abstracts vs ty] substitutes each type variable of [vs] that is
    intersected at top level with abstract atoms in [ty] by the type it is
    intersected with, so that it disappears: [α & array(int)] becomes [array(int)].
    Writing [ty] as [(α & t1) | t2], [α] is substituted by [t1] minus the part already
    covered by [t2] (whose variables are cleaned first): for
    [(α & dict(int,int)) | (β & ref(int)) | array(int)], this gives [α ↦ dict(int,int)]
    and [β ↦ ref(int)].
    Used on the types of parameters and let-bindings when they are added to the
    environment, it prevents them from being refined later into
    intersections of abstract types such as [array(int) & array(bool)]
    (cf. {!Mlsem_system.Config.factorize_abstracts}). *)

val assimilate_vars : MVarSet.t -> GTy.t -> Subst.t
(** [assimilate_vars vs ty] maps each variable of [vs] to another variable of [vs]
    that occurs in [ty] at exactly the same positions and with the same variance,
    if any. A position is a path in the sense of [Sstt.Dependencies.Position]: for
    instance, occurrences in [C('a) & C('b)] or [('a, any) & ('b, any)] are at the
    same position.
    Variables occurring at the top level of a node (or of a record field) together
    with another variable of the same sort, as in ['a & int | 'b & bool], are
    combined by connectives rather than being at the same place: they are never
    assimilated.
    Used on the types of parameters and let-bindings when they are added to the
    environment (cf. {!Mlsem_system.Config.assimilate_vars}). *)
