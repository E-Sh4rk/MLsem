(** Type checker for the functional core language.

    The checker is purely a {e verifier}: it takes an expression together with a
    complete [Annot.t] — the derivation reconstructed by
    {!Mlsem_system.Reconstruction} — and computes the type it proves, raising
    {!Error.Untypeable} if the annotation is not a valid derivation. It never searches
    for an annotation itself. *)

open Mlsem_common
open Mlsem_types
open Annot

val is_type_test_unsat : tau:Ty.t -> GTy.t -> Ty.t
(** [is_type_test_unsat ~tau ty] returns a type that is empty
   if and only if a branch [tau] of a typecase on an expression of type [ty]
   is unreachable *)

val typeof : Env.t -> Annot.t -> Ast.t -> GTy.t
(** [typeof env a e] returns the type that [a] proves for [e] under [env].

    The annotation must {b structurally match} the expression, i.e. have the
    same shape at every node. The result is memoised in the annotation, and the
    cache is {b not} keyed by [env]: a given annotation node must therefore only
    ever be typed under one environment. The reconstruction maintains this by
    only ever applying to an annotation substitutions that are disjoint from the
    environment's variables.

    @raise Error.Untypeable if the annotation is not a valid derivation.
    @raise Assert_failure if the annotation does not match the expression. *)

(** {1 Typing of let-definitions} *)

val generalizable_vars : e:Ast.t -> Env.t -> GTy.t -> MVarSet.t
(** [generalizable_vars ~e env ty] returns the variables of [ty] that can be
    generalized for the definition [e]: those that do not occur in [env], or none
    if [e] is not generalizable under the value restriction
    (see {!Config.value_restriction}). *)

val scheme_of_def : MVarSet.t -> GTy.t -> TyScheme.t
(** [scheme_of_def gen ty] quantifies the variables of [gen] in [ty], then
    simplifies the result with [TyScheme.bot_instance]. Together with the cached
    type of a [Let] definition and its set of generalized variables, this gives
    the scheme of the definition without needing an environment. *)

val generalize : e:Ast.t -> Env.t -> GTy.t -> TyScheme.t
(** [generalize ~e env ty] quantifies the variables of [ty] that do not occur in
    [env], then simplifies the result with [TyScheme.bot_instance]. Under the
    value restriction (see {!Config.value_restriction}), applications are not
    generalized as their evaluation may be effectful.
    Equivalent to [scheme_of_def (generalizable_vars ~e env ty) ty]. *)

val typeof_def : Env.t -> Annot.t -> Ast.t -> TyScheme.t
(** {!typeof} followed by {!generalize}. Used for top-level definitions;
    [Let] definitions are instead generalized according to the set of
    variables stored in their annotation. *)
