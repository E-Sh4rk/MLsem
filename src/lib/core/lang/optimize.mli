(** Dataflow optimization of the minimal imperative language.

    The type system has no notion of mutable state: it types a variable once,
    with a type covering every value the variable may hold. That is too coarse
    for imperative code, where a variable is typically narrowed by a test and
    then used at the narrowed type.

    {!optimize_dataflow} recovers the precision by an SSA-like conversion: every
    write to a mutable variable also binds a fresh {b immutable snapshot}, and
    every read that can be resolved statically is redirected to the snapshot in
    scope. Occurrence typing then applies to the snapshots, which are ordinary
    immutable variables. Mutable cells survive only where the dataflow cannot be
    resolved — across a closure, a loop, or an unknown evaluation order — and
    are dropped altogether when nothing reads them.

    {b Side condition.} The same AST drives the typing rules, so a rewriting
    that is sound for evaluation is not automatically sound here: deleting a
    node also deletes whatever the type system was checking at that node. This
    is why [materialize_annot_obligations] runs first, and any new rewriting
    must be checked against the same criterion.

    The optimization relies on the evaluation orders declared in {!Config}; a
    declared order that the source language does not actually have makes it
    unsound. *)

val optimize_dataflow : MAst.t -> MAst.t
(** The full pipeline: materialize the typing obligations carried by mutable
    declarations, perform the SSA-like conversion, drop the assignments that are
    never read, then {!clean}.
    @raise Failure if the term contains a hole. *)

val clean : MAst.t -> MAst.t
(** Removes definitions and effect-free sub-expressions whose result is
    discarded. Conservative: an expression is only dropped if it can neither
    fail (raise a type error) nor diverge. *)
