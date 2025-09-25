
(** @canonical Mlsem_common.Variable *)
module Variable : sig
  type t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val compare : t -> t -> int
  val equals : t -> t -> bool
  val create : string option -> t
  val refresh : t -> t
  val attach_location : t -> Position.t -> unit
  val get_location : t -> Position.t
  val get_name : t -> string option
  val get_unique_name : t -> string
end

(** @canonical Mlsem_common.VarMap *)
module VarMap : Map.S with type key=Variable.t

(** @canonical Mlsem_common.VarSet *)
module VarSet : Set.S with type elt=Variable.t
