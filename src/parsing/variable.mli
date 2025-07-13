module Variable : sig
  type t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val compare : t -> t -> int
  val equals : t -> t -> bool
  val create_let : string option -> t
  val create_lambda : string option -> t
  val create_gen : string option -> t
  val attach_location : t -> Position.t -> unit
  val get_location : t -> Position.t
  val is_let_var : t -> bool
  val is_lambda_var : t -> bool
  val get_name : t -> string option
  val get_unique_name : t -> string
end

module VarMap : Map.S with type key=Variable.t
module VarSet : Set.S with type elt=Variable.t
