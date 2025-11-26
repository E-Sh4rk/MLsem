open Mlsem_common
open Annot

val initial : REnvSet.t -> Ast.t -> IAnnot.t

(* Can raise Checker.Untypeable *)
val refine : Env.t -> IAnnot.t -> Ast.t -> Annot.t

(* Can raise Checker.Untypeable *)
val infer : Env.t -> REnvSet.t -> Ast.t -> Annot.t
