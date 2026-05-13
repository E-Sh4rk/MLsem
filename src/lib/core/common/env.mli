open Var
open Mlsem_types

module type Env = sig
    type t
    type ty
    val empty : t
    val is_empty : t -> bool
    val singleton : Variable.t -> ty -> t
    val construct : (Variable.t * ty) list -> t
    val add : Variable.t -> ty -> t -> t
    val replace : Variable.t -> ty -> t -> t
    val domain : t -> Variable.t list
    val bindings : t -> (Variable.t * ty) list
    val mem : Variable.t -> t -> bool
    val find : Variable.t -> t -> ty
    val find_opt : Variable.t -> t -> ty option
    val rm : Variable.t -> t -> t
    val rms : Variable.t list -> t -> t
    val restrict : Variable.t list -> t -> t
    val map : (ty -> ty) -> t -> t
    val filter : (Variable.t -> ty -> bool) -> t -> t
    val tvars : t -> MVarSet.t
    val substitute : Subst.t -> t -> t

    val equiv : t -> t -> bool
    val leq : t -> t -> bool

    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val pp_filtered : string list -> Format.formatter -> t -> unit
end

(** @canonical Mlsem_common.Env *)
module Env : Env with type ty:=TyScheme.t

(** @canonical Mlsem_common.REnv *)
module REnv : sig
  include Env with type ty:=Ty.t
  val find' : Variable.t -> t -> Ty.t
  val cap : t -> t -> t
  val conj : t list -> t
  val neg : t -> t list
  val cup_approx : t -> t -> t
  val disj_approx : t list -> t
  val neg_approx : t -> t option
  val refine_env : Env.t -> t -> Env.t
end
