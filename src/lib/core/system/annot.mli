(** Typing derivations, in the two forms the reconstruction works with.

    A derivation mirrors the shape of the expression it types and supplies, at
    each node, the choices the typing rules cannot make on their own: which
    instance of a polymorphic variable to take, which type to give a lambda's
    parameter, which branches of a typecase to explore, and so on.

    {!Annot} is a {b complete} derivation, the input of {!Mlsem_system.Checker}.
    {!IAnnot} is an {b intermediate} one, in which some choices are still
    undecided; it embeds completed sub-derivations through its [A] constructor,
    so the reconstruction gradually rewrites [I] nodes into [A] ones. *)

open Mlsem_common
open Mlsem_types

(** A complete derivation. *)
module Annot : sig
  type branch = BType of t | BSkip
  (** A branch of a typecase: either typed, or skipped because it was shown
      unreachable. *)

  and inter = t list
  (** Several derivations for a same expression; the expression receives the
      intersection of the types they prove. This is how overloaded types are
      obtained. *)

  and part = (Ty.t * t option) list
  (** One derivation of the body of a [Let] per cell of the bound variable's
      decomposition. [None] means the cell is empty and needs no derivation. *)

  and a =
  | AValue of GTy.t
  | AVar of Subst.t
  | AConstruct of t list
  | ALet of t * part
  | ALet' of t * t
  | AApp of t * t * Ty.t (* result *)
  | AOp of GTy.t * t * Ty.t (* result *)
  | AProj of t
  | ACast of GTy.t * t
  | ACoerce of GTy.t * t
  | AIte of t * GTy.t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AAlt of t option list
  | AInter of inter
  and t = { mutable cache: GTy.t option ; ann: a ; refinement: REnv.t }
  (** [refinement] narrows the environment before typing this node, and [cache]
      memoises the resulting type.

      The cache is {b not keyed by the environment}: a node must therefore only
      ever be typed under one environment. The reconstruction upholds this by
      only applying to a derivation substitutions whose domain is disjoint from
      the environment's variables — which is also what keeps a cached type valid
      after [substitute]. *)

  val nc : REnv.t -> a -> t
  (** Builds a node with an empty cache ("no cache"). *)

  val substitute : Subst.t -> t -> t
  (** Substitutes throughout the derivation, cached types included. *)

  val pp : Format.formatter -> t -> unit
  val pp_a : Format.formatter -> a -> unit
end

(** Identifies the node a result type comes from, so that two results are only
    compared when they constrain the same node. *)
module Rid (* Result identifier *) : sig
  type t
  val dummy : t
  val create : unit -> t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

(** An intermediate derivation, still containing undecided choices. *)
module rec IAnnot : sig
  type res = (Rid.t * Ty.t) option
  (** The result type a branch of an intersection was explored for, tagged with
      the node that produced it. [None] is the default branch, which assumes
      nothing. *)

  type coverage = res * REnv.t
  (** What a branch of an intersection covers: the result it was explored for,
      and the refinement of the environment under which it was explored. Used to
      skip branches subsumed by those already explored (cf. {!Domain}). *)

  type branch = BMaybe of t | BType of t | BSkip
  (** A branch of a typecase: [BMaybe] before it has been decided whether the
      branch is reachable, [BType] / [BSkip] afterwards. *)

  and inter_branch = { coverage: coverage option ; ann: t option }
  (** One branch of an intersection; [ann = None] once the branch has been found
      untypeable. *)

  and inter = inter_branch list
  and part = (Ty.t * LazyIAnnot.t option) list
  and a =
  | Untyp (** Known untypeable; fails immediately. *)
  | AVar of (MVarSet.t -> Subst.t)
  | AConstruct of t list
  | ALet of t * part
  | ALet' of t * t
  | AApp of t * t * Ty.t (* result *)
  | AOp of (MVarSet.t -> Subst.t) * t * Ty.t (* result *)
  | AProj of t * Ty.t (* result *)
  | ACast of GTy.t * t
  | ACoerce of GTy.t * t
  | AIte of t * GTy.t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AAlt of bool (* masked *) * t option list
  | AInter of inter
  and t =
  | A of Annot.t (** An already completed sub-derivation. *)
  | I of { rid: Rid.t ; ann: a ; refinement: REnv.t }

  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val pp_a : Format.formatter -> a -> unit
  val pp_coverage : Format.formatter -> coverage -> unit
  val pp_res : Format.formatter -> res -> unit
end

(** A derivation whose construction is deferred.

    Used for the cells of a partitioned [Let]: building the derivation of the
    body for every cell up-front is wasteful, since the reconstruction may never
    explore some of them.

    Forcing is shared, but pending substitutions are not: [substitute] returns
    a new handle onto the same deferred derivation, with the substitution queued
    on it. So two handles obtained from a same origin force it at most once, and
    each then applies its own substitutions to the result. *)
and LazyIAnnot : sig
  type t

  val get : t -> IAnnot.t
  (** Forces the derivation and applies the substitutions queued on this
      handle. *)

  val mk_lazy : (unit -> IAnnot.t) -> t
  val mk : IAnnot.t -> t

  val is_concrete : t -> bool
  (** Whether the underlying derivation has already been forced — {b not}
      whether {!get} would be free, as this handle may still carry pending
      substitutions. *)

  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
end

(** The part of an expression's typing already covered by the branches of an
    intersection, used to avoid exploring a branch that would add nothing.

    An environment refinement is compared by encoding it as an open record type
    — one field per constrained variable — which turns "is this refinement
    within the union of those already explored" into a subtyping test. *)
module Domain : sig
    type t
    val empty : t
    val add : IAnnot.coverage -> t -> t

    val covers : t -> IAnnot.coverage -> bool
    (** Whether the recorded coverages already subsume the given one, i.e.
        whether every environment it applies to is handled by a branch already
        explored for a result at least as precise. Results are only comparable
        when they come from the same node (same {!Rid.t}). *)

    val pp : Format.formatter -> t -> unit
end
