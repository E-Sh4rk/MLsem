(** Compilation of the full language down to the functional core.

    {!Ast} has pattern matching, structured statements ([if]/[while]), non-local
    exits ([return]/[break]) and mutable variables. {!Mlsem_system.Ast} has none
    of these. {!transform} bridges the gap in three stages:

    - {!eliminate_cf} removes the control flow, producing an {!MAst.t}: pattern
      matching becomes a chain of typecases over a partitioned [let], [if] and
      [while] become [Ite] inside a [Loop], and [return]/[break] are eliminated
      by duplicating the code that follows them into each exit path. A [Ret]
      that cannot be eliminated that way — because it sits under a lambda, a
      loop or an unknown evaluation order — falls back to writing a mutable
      variable that the enclosing block reads back. {!Ast.Isolate} marks a
      sub-expression whose surrounding context must not be duplicated.
    - {!Optimize.optimize_dataflow} turns the remaining mutable variables into
      immutable snapshots wherever the dataflow allows.
    - {!MAst.to_system_ast} encodes what is left — the surviving mutable cells —
      as operations on an abstract reference type.

    The order matters: control flow must go before the dataflow analysis, which
    reasons about a single expression tree, and the reference encoding must come
    last, since it hides assignments inside opaque operations.

    Both functions expect an alpha-converted term whose nodes carry distinct
    [Mlsem_common.Eid.t]s, and preserve that property: every sub-expression
    they duplicate gets refreshed ids. *)

val eliminate_cf : Ast.t -> MAst.t
(** Removes pattern matching and control flow.
    @raise Invalid_argument on an ill-formed pattern (cf. {!Ast.pattern}) or on
    a [Ret] that refers to no enclosing block. *)

val transform : Ast.t -> Mlsem_system.Ast.t
(** {!eliminate_cf}, then {!Optimize.optimize_dataflow}, then
    {!MAst.to_system_ast}. *)
