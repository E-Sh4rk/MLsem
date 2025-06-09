open Parsing.Variable
open Types
open Types.Base
open Types.Tvar

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

module Make(T:T) : Env with type ty:=T.t
module Env : Env with type ty:=TyScheme.t
module REnv : sig
  include Env with type ty:=typ
  val cap : t -> t -> t
  val conj : t list -> t
end
