open Mlsem_common
open Annot

(* Can raise Checker.Untypeable *)
val infer : Env.t -> REnvSet.t -> Ast.t -> Annot.t
