open Mlsem_common
open Mlsem_types

val typeof_def : Env.t -> Ast.t -> TyScheme.t
(** [typeof_def env e] returns an approximation of the type of the definition
    [e] under the environment [env]. This approximation is [TyScheme.any] in most cases,
    but it can be more precise for simple definitions used to encode pattern matching
    (variables, casts, projections) or in the presence of user type annotations (coercions). *)

val refine : Env.t -> Ast.t -> Ty.t -> REnv.t
val refinement_envs :
  ?extra_checks:(Eid.t * Ty.t) list ->
  ?refine_on_typecases:bool ->
  ?refine_on_casts:bool ->
  Env.t -> Ast.t -> REnvSet.t

module Partitioner : sig
  type t
  val from_renvset : REnvSet.t -> t
  val filter_compatible : t -> Variable.t -> Ty.t -> t
  val partition_for : t -> Variable.t -> Ty.t list -> Ty.t list
end
