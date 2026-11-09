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
  | TagOpaque of Tag.t
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

type printer_param =
| Tagged of Tag.t * printer
(** [Tagged (tag, p)]: the witnesses [Tag (tag, w)] are printed by [p]. *)
| Opaque of Tag.t * string
(** [Opaque (tag, str)]: the witnesses [TagOpaque tag] are printed as [str]. *)

val add_printer_param : printer_param -> unit
(** Registers a printer, with priority over the printers registered before. *)

val set_printer_params : printer_param list -> unit
(** Replaces the printers registered with {!add_printer_param}.
    Built-in printers are always kept, but can be overridden. *)

val add_builtin_printer_param : printer_param -> unit
(** Registers a built-in printer: it is not removed by {!set_printer_params}.
    Built-in printers are already registered for the extensions of Sstt
    (booleans, floats, characters, strings and lists), and for abstract
    types defined by {!Mlsem_types.Abstract.define}. *)

(** Default printer for witnesses, using the registered printers.
    A tagged witness without a registered printer is printed as [tag(w)],
    or [tag(_)] for [TagOpaque tag]. In records, absent fields are omitted,
    and the witness for all other labels is printed as [_ = w]. *)
val pp : Format.formatter -> t -> unit
