(** Extraction of witnesses (sample inhabitants) from non-empty types. *)

open Sstt

(** A witness: a description of a value inhabiting a type. *)
type t =
  | Other  (** Any value (e.g. of a component that is entirely included) *)
  | Int of Z.t  (** An integer *)
  | Enum of Enum.t
  (** An enum constant, possibly {!enum_default} for a constant
      not mentioned in the type *)
  | Tuple of t list  (** A tuple *)
  | Record of (Label.t * tf) list * tf
  (** A record: explicit fields and the field for all other labels *)
  | Tag of Tag.t * t  (** A tagged value *)
  | TagOther of Tag.t
  (** A value with the given tag, whose content cannot be described
      (the tag does not preserve intersections) *)
  | Arrow  (** A function *)

(** A (possibly absent) record field. *)
and tf =
  | Absent
  | Present of t

val enum_default : Enum.t
(** The enum constant used as a witness for co-finite sets of constants
    (e.g. the type [any \ (A | B)]).
    {b Invariant:} this constant must not appear in the types given to
    {!of_ty}: it is assumed to be distinct from every constant mentioned
    in them (this is checked by an assertion). *)

(** [of_ty ty] returns a witness of minimal depth for [ty],
    or [None] if [ty] is empty. Type and row variables are ignored. *)
val of_ty : Ty.t -> t option

(** {1 Printing} *)

type printer = Format.formatter -> t -> unit
(** A printer for tagged values, which is given the content of the tag
    (i.e. [w] for the witness [Tag (tag, w)]). *)

val add_printer_param : Tag.t * printer -> unit
(** [add_printer_param (tag, p)] registers [p] as the printer for the values
    tagged with [tag], with priority over the printers registered before. *)

val set_printer_params : (Tag.t * printer) list -> unit
(** Replaces the printers registered with {!add_printer_param}.
    Built-in printers (for strings, floats and lists) are always kept, but can be overridden. *)

(** Default printer for witnesses. Values tagged with a tag having a registered
    printer are printed with it. Values of other extensions (booleans,
    characters, etc.) are printed as their type, between [<...>] if the value is not
    fully specified. In records, absent fields are omitted, and the witness
    for all other labels is printed as [_ = w]. *)
val pp : Format.formatter -> t -> unit
