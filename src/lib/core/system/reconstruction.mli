open Mlsem_common
open Annot
open Refinement

val initial : ?direct_narrowing:bool -> Refinements.t -> Ast.t -> IAnnot.t

(* Can raise Checker.Untypeable *)
val refine : Env.t -> IAnnot.t -> Ast.t -> Annot.t

(* Can raise Checker.Untypeable *)
val infer : ?direct_narrowing:bool -> Env.t -> Refinements.t -> Ast.t -> Annot.t
