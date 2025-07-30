open Common
open Annot
open Types

module Domain : sig
    type t
    val empty : t
    val add : IAnnot.coverage -> t -> t
    val covers : t -> IAnnot.coverage -> bool
    val pp : Format.formatter -> t -> unit
end

module TVCache : sig
    type t
    val empty : unit -> t
    val get : t -> Eid.t -> TVar.t -> TVar.t
    val get' : t -> Eid.t -> TVarSet.t -> Subst.t
    val get_abs_param : t -> Abstract.t -> int -> TVar.t -> TVar.t
    val res_tvar : TVar.t
    val res_tvar' : int -> TVar.t
end