open Types.Base
open Types.Tvar

module Annot : sig
  type branch = BType of t | BSkip
  and inter = t list
  and part = (typ * t) list
  and t =
  | AConst | AAbstract | AAtom
  | AAx of Subst.t
  | ALet of t * part
  | AApp of t * t | ACons of t * t
  | AProj of t | ATag of t
  | AUpdate of t * t option
  | ATuple of t list
  | AIte of t * branch * branch
  | ALambda of typ * t
  | AInter of inter

  val substitute : Subst.t -> t -> t
end

module IAnnot : sig
  type branch = BType of t | BSkip | BInfer
  and inter = t list
  and part = (typ * t) list
  and t =
  | A of Annot.t
  | Infer
  | Untyp
  | AConst | AAbstract | AAtom
  | AAx of Subst.t
  | ALet of t * part
  | AApp of t * t | ACons of t * t
  | AProj of t | ATag of t
  | AUpdate of t * t option
  | ATuple of t list
  | AIte of t * branch * branch
  | ALambda of typ * t
  | AInter of inter

  val substitute : Subst.t -> t -> t
end