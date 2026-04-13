open Mlsem_common
open Mlsem_types

module Refinements : sig
  type t
  val empty : t
  val get : t -> Eid.t -> REnv.t
  val get_anonymous : t -> REnv.t list
  val all : t -> REnv.t list
  val add : t -> Eid.t -> REnv.t -> t
  val add_anonymous : t -> REnv.t -> t
  val map : (REnv.t -> REnv.t) -> t -> t
end

val typeof_def : Env.t -> Ast.t -> TyScheme.t
(** [typeof_def env e] returns an approximation of the type of the definition
    [e] under the environment [env]. This approximation is [TyScheme.any] in most cases,
    but it can be more precise for simple definitions used to encode pattern matching
    (variables, casts, projections) or in the presence of user type annotations (coercions). *)

val refine : Env.t -> Ast.t -> Ty.t -> REnv.t
val refinements :
  ?extra_checks:(Eid.t * Ty.t) list ->
  ?refine_on_typecases:bool ->
  ?refine_on_casts:bool ->
  Env.t -> Ast.t -> Refinements.t

module Partitioner : sig
  type t
  val from_refinements : Refinements.t -> t
  val filter_compatible : t -> Variable.t -> Ty.t -> t
  val decomposition_for : t -> Variable.t -> Ty.t list -> Ty.t list
end
