open Common
open Types

module Annot : sig
  type branch = BType of t | BSkip
  and inter = t list
  and part = (Ty.t * t) list
  and a =
  | AConst
  | AAbstract of GTy.t
  | AAx of Subst.t
  | AConstruct of t list
  | ALet of t * part
  | AApp of t * t
  | AProj of t
  | ACast of t
  | ACoerce of GTy.t * t
  | AIte of t * branch * branch
  | ACf of t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AInter of inter
  and t = { mutable cache: GTy.t option ; ann: a }

  val nc : a -> t
  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val pp_a : Format.formatter -> a -> unit
end

module IAnnot : sig
  type coverage = (Eid.t * Ty.t) option * REnv.t
  type branch = BType of t | BSkip | BInfer
  and inter_branch = { coverage: coverage option ; ann: t }
  and inter = inter_branch list
  and part = (Ty.t * t) list
  and t =
  | A of Annot.t
  | Infer
  | Untyp
  | AConstruct of t list
  | ALet of t * part
  | AApp of t * t
  | AProj of t
  | ACast of t
  | ACoerce of GTy.t * t
  | AIte of t * branch * branch
  | ACf of t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AInter of inter

  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val pp_coverage : Format.formatter -> coverage -> unit
end