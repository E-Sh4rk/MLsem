open Mlsem_common
open Annot
open Mlsem_types

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
    val res_tvar : TVar.t
end