open Mlsem_common
open Mlsem_types

module Annot : sig
  type branch = BType of t | BSkip
  and inter = t list
  and part = (Ty.t * t) list
  and a =
  | AValue of GTy.t
  | AVar of Subst.t
  | AConstruct of t list
  | ALet of t * part
  | AApp of t * t
  | AOp of Subst.t * t
  | AProj of t
  | ACast of GTy.t * t
  | ACoerce of GTy.t * t
  | AIte of t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AAlt of t option * t option
  | AInter of inter
  and t = { mutable cache: GTy.t option ; ann: a }

  val nc : a -> t
  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val pp_a : Format.formatter -> a -> unit
end

module IAnnot : sig
  type coverage = (Eid.t * Ty.t) option * REnv.t
  type branch = BType of bool (* explored *) * t | BSkip
  and inter_branch = { coverage: coverage option ; ann: t }
  and inter = inter_branch list
  and part = (Ty.t * t) list
  and t =
  | A of Annot.t
  | Untyp
  | AVar of (MVarSet.t -> Subst.t)
  | AConstruct of t list
  | ALet of t * part
  | AApp of t * t * Ty.t (* result *)
  | AOp of (MVarSet.t -> Subst.t) * t * Ty.t (* result *)
  | AProj of t * Ty.t (* result *)
  | ACast of GTy.t * t
  | ACoerce of GTy.t * t
  | AIte of t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AAlt of t option * t option
  | AInter of inter

  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val pp_coverage : Format.formatter -> coverage -> unit
end

module Domain : sig
    type t
    val empty : t
    val add : IAnnot.coverage -> t -> t
    val covers : t -> IAnnot.coverage -> bool
    val pp : Format.formatter -> t -> unit
end
