open Env
open Types.Base
open Parsing.Variable

val refine : Env.t -> Ast.t -> typ -> REnv.t
val refinement_envs : Env.t -> Ast.t -> REnvSet.t

module Partitioner : sig
  type t
  val from_renvset : REnvSet.t -> t
  val filter_compatible : t -> Variable.t -> typ -> t
  val partition_for : t -> Variable.t -> typ list -> typ list
end
