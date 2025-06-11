open Env
open Annot

val infer : Env.t -> REnvSet.t -> Ast.t -> Annot.t option
