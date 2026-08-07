
(** Program variables.

    Variables are identified by identity, never by name: two variables with the
    same display name are distinct. Terms are therefore expected to be already
    alpha-converted, which is what lets [Mlsem_system.Ast.fv] compute free
    variables as "used minus bound" over a whole term.

    @canonical Mlsem_common.Variable *)
module Variable : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val create : string option -> t
  (** Fresh variable with an optional display name, no location. *)

  val refresh : t -> t
  (** Fresh variable with the same display name and locations as its argument.
      The result is a {e distinct} variable that {!pp} prints identically — use
      {!pp_uniq} to tell copies apart. *)

  val attach_location : t -> Position.t -> unit
  (** Sets the definition site of the variable (there is only one). *)

  val attach_sig_location : t -> Position.t -> unit
  (** Adds a declaration site (a [val] declaration); a variable may have
      several. *)

  val get_location : t -> Position.t
  (** {!Position.dummy} if no location was attached. *)

  val get_sig_locations : t -> Position.t list
  val get_name : t -> string option

  val pp : Format.formatter -> t -> unit
  (** Prints the display name, which is {e not} injective (see {!refresh}). *)

  val pp_uniq : Format.formatter -> t -> unit
  (** Prints the display name suffixed by the variable's unique id. *)

  val show : t -> string
  val show_uniq : t -> string
end

(** @canonical Mlsem_common.VarMap *)
module VarMap : Map.S with type key=Variable.t

(** @canonical Mlsem_common.VarSet *)
module VarSet : Set.S with type elt=Variable.t
