(** Tuning knobs of the type system.

    These are global mutable settings, read at each use rather than captured, so
    changing one affects every subsequent type-checking operation. *)

open Mlsem_types

(** When [true], the type of applications is not generalized
    as their evaluation may be effectful (cf. [Checker.generalize]).
    Disabling it makes inference more permissive but unsound
    in the presence of mutable state. *)
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
    be considered uninhabited (cf. [Heuristics.normalize_empty_abstracts]).
    Use [Fun.id] for no normalization. *)
let normalization_fun : (Ty.t -> Ty.t) ref = ref Heuristics.normalize_empty_abstracts

(** Normalization applied to the set of solutions of a tallying instance. It may
    split, reorder or drop solutions, but every returned substitution must be a
    solution of the instance: the reconstruction relies on them being sound, not
    on them being complete.
    Use [fun _ x -> x] for no normalization (the default). *)
let subst_normalization_fun : (Heuristics.tally_context -> Subst.t list -> Subst.t list) ref =
  ref (fun _ x -> x)

(** Whether, when the (partially inferred) type of a parameter or of a
    let-binding is added to the environment, its inference variables
    that occur at the same positions and with the same variance should be identified
    (cf. [Heuristics.assimilate_vars]). This loses generality, but only for types
    that are inferred: annotated parameters are not affected. *)
let assimilate_vars = ref true

(** Whether, when the (partially inferred) type of a parameter or of a
    let-binding is added to the environment, its inference variables
    intersected with abstract types should be substituted by the type they are
    intersected with (cf. [Heuristics.factorize_abstracts]). This prevents the
    inference of intersections of abstract types such as [array(int) & array(bool)]. *)
let factorize_abstracts = ref true

(** Custom simplification applied, after the ones above, to the (partially inferred)
    type of a parameter or of a let-binding when it is added to the environment.
    It is given whether the type is the one of a parameter ([~is_lambda:true]) or of
    a let-binding ([~is_lambda:false]), the variables that may be substituted (the
    inference variables of the type that are neither generalized nor bound by the
    environment) and the type, and returns a substitution, [Subst.identity] meaning
    that there is nothing to simplify.
    Any such substitution yields a valid derivation, but it may lose generality.
    When the substitution is not the identity, it is applied and the binding is
    typed again, so the function must eventually return [Subst.identity].
    For instance, [fun ~is_lambda:_ -> Heuristics.assimilate_vars] reimplements
    [assimilate_vars]. Use [fun ~is_lambda:_ _ _ -> Subst.identity] for no
    simplification (the default). *)
let var_simplification_fun : (is_lambda:bool -> MVarSet.t -> GTy.t -> Subst.t) ref =
  ref (fun ~is_lambda:_ _ _ -> Subst.identity)
