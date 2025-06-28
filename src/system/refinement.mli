open Env
open Types.Base
open Parsing.Variable

val refine : Env.t -> Ast.t -> typ -> REnv.t
val refinement_envs : Env.t -> Ast.t -> REnvSet.t

module FilteredREnvSet : sig
  type t
  val from_renvset : REnvSet.t -> t
  val filter_compatible : t -> Variable.t -> typ -> t
  val elements : t -> REnv.t list
end
