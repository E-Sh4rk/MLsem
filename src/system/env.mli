open Parsing.Variable
open Types.Base
open Types.Tvar

module TyScheme : sig
    type t
    val mk : TVarSet.t -> typ -> t
    val mk_mono : typ -> t
    val mk_poly : typ -> t
    val get : t -> TVarSet.t * typ
    val fv : t -> TVarSet.t
    val leq : t -> t -> bool
    val equiv : t -> t -> bool
    val leq_inst : t -> t -> bool
    val equiv_inst : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val pp_short : Format.formatter -> t -> unit
end

module Env : sig
    type t
    val empty : t
    val is_empty : t -> bool
    val singleton : Variable.t -> TyScheme.t -> t
    val construct : (Variable.t * TyScheme.t) list -> t
    val add : Variable.t -> TyScheme.t -> t -> t
    val domain : t -> Variable.t list
    val bindings : t -> (Variable.t * TyScheme.t) list
    val mem : Variable.t -> t -> bool
    val find : Variable.t -> t -> TyScheme.t
    val rm : Variable.t -> t -> t
    val rms : Variable.t list -> t -> t
    val restrict : Variable.t list -> t -> t
    val map : (TyScheme.t -> TyScheme.t) -> t -> t
    val filter : (Variable.t -> TyScheme.t -> bool) -> t -> t
    val tvars : t -> TVarSet.t

    val equiv : t -> t -> bool
    val leq : t -> t -> bool

    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val pp_filtered : string list -> Format.formatter -> t -> unit
end
