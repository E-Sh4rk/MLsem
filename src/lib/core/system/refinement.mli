open Common
open Types

val refine : Env.t -> Ast.t -> Ty.t -> REnv.t
val refinement_envs : Env.t -> Ast.t -> REnvSet.t
val partition : Ty.t list -> Ty.t list

module Partitioner : sig
  type t
  val from_renvset : REnvSet.t -> t
  val filter_compatible : t -> Variable.t -> Ty.t -> t
  val partition_for : t -> Variable.t -> Ty.t list -> Ty.t list
end
