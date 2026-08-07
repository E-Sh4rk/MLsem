(** Reconstruction of typing derivations.

    {!Mlsem_system.Checker} can verify a derivation but not find one. This
    module searches for one: starting from a skeleton in which every choice is
    undecided, it repeatedly picks a node it cannot type as is, asks the
    constraint solver which instantiations of the inference variables would make
    it typeable, and commits to them.

    {b The search.} Refining a node returns one of three outcomes:
    - it typechecks as is, and the sub-derivation is complete;
    - it is untypeable, and so is the whole derivation;
    - it would typecheck under one of several {e substitutions} of the inference
      variables.

    In the last case the node cannot decide alone, because the substitutions
    also constrain the rest of the expression. Two things can happen:

    - if some substitution touches a variable of the {b environment}, the
      requirement is propagated outwards: the enclosing expression is what gets
      to choose, since applying the substitution to this sub-derivation alone
      would leave it inconsistent with the environment;
    - otherwise the substitutions only concern variables local to this
      expression, and the node is turned into an {b intersection}: one branch
      per substitution, plus a default branch for "none of them applies". Each
      branch is then explored independently and the results are intersected,
      which is how overloaded types arise. Branches subsumed by an
      already-explored one are skipped (cf. {!Annot.Domain}).

    That side condition is also what keeps the cached types of [Annot.t] valid:
    a substitution is only ever applied to a derivation when it cannot affect
    the environment that derivation was typed under.

    {b Termination.} Every substitution outcome returns derivations that are
    strictly more decided than the one it was given — an undecided node has
    become a completed sub-derivation, a resolved branch, or [Untyp] — so the
    re-exploration cannot cycle. Any change to the search must preserve this. *)

open Mlsem_common
open Annot
open Refinement

val initial : ?direct_narrowing:bool -> ?partition_narrowing:bool
    -> Refinements.t -> Ast.t -> IAnnot.t
(** The skeleton derivation of an expression, in which every choice is left
    undecided. The two flags select how the precomputed refinements are used:
    [direct_narrowing] attaches them to program points, [partition_narrowing]
    turns them into decompositions of let-bound variables (cf. {!Refinement}).
    Both default to [true]. *)

val refine : Env.t -> IAnnot.t -> Ast.t -> Annot.t
(** Searches for a complete derivation, starting from the given skeleton.
    @raise Checker.Untypeable if the expression cannot be typed; the error
    points at the most recent operation the search failed on.
    @raise Failure if the search ends up requiring a substitution of a variable
    of [env], which can only happen if the environment was not closed. *)

val infer : ?direct_narrowing:bool -> ?partition_narrowing:bool
    -> Env.t -> Refinements.t -> Ast.t -> Annot.t
(** {!initial} followed by {!refine}.
    @raise Checker.Untypeable if the expression cannot be typed.
    @raise Failure as {!refine}. *)
