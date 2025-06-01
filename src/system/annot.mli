open Types.Base
open Types.Tvar
open Env

module Annot : sig
  type branch = BType of t | BSkip
  and inter = t list
  and part = (typ * t) list
  and a =
  | AConst | AAbstract | AAtom
  | AAx of Subst.t
  | ALet of t * part
  | AApp of t * t | ACons of t * t
  | AProj of t | ATag of t | AConstr of t | ACoerce of typ * t
  | AUpdate of t * t option
  | ATuple of t list
  | AIte of t * branch * branch
  | ALambda of typ * t
  | AInter of inter
  and t = { mutable cache: typ option ; ann: a }

  val nc : a -> t
  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val pp_a : Format.formatter -> a -> unit
end

module IAnnot : sig
  type coverage = (Parsing.Ast.exprid * typ) option * REnv.t
  type branch = BType of t | BSkip | BInfer
  and inter_branch = { coverage: coverage option ; ann: t }
  and inter = inter_branch list
  and part = (typ * t) list
  and t =
  | A of Annot.t
  | Infer
  | Untyp
  | AConst | AAbstract | AAtom
  | AAx of Subst.t
  | ALet of t * part
  | AApp of t * t | ACons of t * t
  | AProj of t | ATag of t | AConstr of t | ACoerce of typ * t
  | AUpdate of t * t option
  | ATuple of t list
  | AIte of t * branch * branch
  | ALambda of typ * t
  | AInter of inter

  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val pp_coverage : Format.formatter -> coverage -> unit
end