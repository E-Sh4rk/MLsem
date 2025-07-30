open Variable
open Types

module type T = sig
    type t
    val fv : t -> TVarSet.t
    val leq : t -> t -> bool
    val substitute : Subst.t -> t -> t
    val pp : Format.formatter -> t -> unit
  end  

module type Env = sig
    type t
    type ty
    val empty : t
    val is_empty : t -> bool
    val singleton : Variable.t -> ty -> t
    val construct : (Variable.t * ty) list -> t
    val add : Variable.t -> ty -> t -> t
    val domain : t -> Variable.t list
    val bindings : t -> (Variable.t * ty) list
    val mem : Variable.t -> t -> bool
    val find : Variable.t -> t -> ty
    val rm : Variable.t -> t -> t
    val rms : Variable.t list -> t -> t
    val restrict : Variable.t list -> t -> t
    val map : (ty -> ty) -> t -> t
    val filter : (Variable.t -> ty -> bool) -> t -> t
    val tvars : t -> TVarSet.t
    val substitute : Subst.t -> t -> t

    val equiv : t -> t -> bool
    val leq : t -> t -> bool

    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val pp_filtered : string list -> Format.formatter -> t -> unit
end

module Env : Env with type ty:=TyScheme.t

module REnv : sig
  include Env with type ty:=Ty.t
  val find' : Variable.t -> t -> Ty.t
  val cap : t -> t -> t
  val conj : t list -> t
  val neg : t -> t list
  val cup_approx : t -> t -> t
  val disj_approx : t list -> t
  val neg_approx : t -> t option
end

module REnvSet : sig
  type t
  val empty : t
  val of_list : REnv.t list -> t
  val add : t -> REnv.t -> t
  val union : t -> t -> t
  val elements : t -> REnv.t list
end
