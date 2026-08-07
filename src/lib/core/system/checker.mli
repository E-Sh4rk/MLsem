(** Type checker for the functional core language.

    The checker is purely a {e verifier}: it takes an expression together with a
    complete [Annot.t] — the derivation reconstructed by
    {!Mlsem_system.Reconstruction} — and computes the type it proves, raising
    {!Untypeable} if the annotation is not a valid derivation. It never searches
    for an annotation itself. *)

open Mlsem_common
open Mlsem_types
open Annot

val is_type_test_unsat : tau:GTy.t -> GTy.t -> Ty.t
(** [is_type_test_unsat ~tau ty] returns a type that is empty
   if and only if a branch [tau] of a typecase on an expression of type [ty]
   is unreachable *)

type error = { eid: Eid.t ; title: string ; descr: string option }
exception Untypeable of error
(** Raised when the annotation is not a valid derivation for the expression.
    In normal operation the reconstruction only produces valid annotations, so
    this escaping from {!typeof} indicates an internal inconsistency rather than
    a user type error — user errors are reported by the reconstruction. *)

val typeof : Env.t -> Annot.t -> Ast.t -> GTy.t
(** [typeof env a e] returns the type that [a] proves for [e] under [env].

    The annotation must {b structurally match} the expression, i.e. have the
    same shape at every node. The result is memoised in the annotation, and the
    cache is {b not} keyed by [env]: a given annotation node must therefore only
    ever be typed under one environment. The reconstruction maintains this by
    only ever applying to an annotation substitutions that are disjoint from the
    environment's variables.

    @raise Untypeable if the annotation is not a valid derivation.
    @raise Assert_failure if the annotation does not match the expression. *)

val generalize : e:Ast.t -> Env.t -> GTy.t -> TyScheme.t
(** [generalize ~e env ty] quantifies the variables of [ty] that do not occur in
    [env], then simplifies the result with [TyScheme.bot_instance]. Under the
    value restriction (see {!Config.value_restriction}) nothing is quantified
    unless [e] is a generalizable expression, i.e. one whose evaluation cannot
    have an effect. *)

val typeof_def : Env.t -> Annot.t -> Ast.t -> TyScheme.t
(** {!typeof} followed by {!generalize}. *)
