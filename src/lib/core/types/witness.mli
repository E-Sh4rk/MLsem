(** Extraction of witnesses (sample inhabitants) from non-empty types. *)

open Sstt

(** A witness: a description of a value inhabiting a type.

    The [*Default] witnesses stand for a value of a part of the type that is an
    infinite union of components (e.g. a negative type such as [any] or [~int]).
    Such parts are not explored, as they may include components that do not
    correspond to values of the language (e.g. integers or tuples of some arity
    in a language without them). Choosing a representative for them is left to
    the printer. *)
type t =
  | Default
  (** Any value not explicitly mentioned in the type (negative types) *)
  | Int of Z.t  (** An integer *)
  | Enum of Enum.t  (** An enum constant *)
  | EnumDefault  (** An enum constant not mentioned in the type *)
  | Tuple of t list  (** A tuple *)
  | TupleDefault  (** A tuple whose arity is not mentioned in the type *)
  | Record of (Label.t * tf) list * tf
  (** A record: explicit fields and the field for all other labels *)
  | Tag of Tag.t * t  (** A tagged value *)
  | TagOpaque of Tag.t
  (** A value with the given tag, whose content cannot be described
      (the tag does not preserve intersections) *)
  | TagDefault  (** A tagged value whose tag is not mentioned in the type *)
  | Arrow  (** A function *)

(** A (possibly absent) record field. *)
and tf =
  | Absent
  | Present of t

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

(** {2 Default witnesses}

    The representatives printed for default witnesses. They are arbitrary, and
    should be chosen to be unlikely to be excluded by the types (they are only
    used for error reporting). *)

val set_default_printer : string -> unit
(** Representative for {!Default} (default: [_]). *)

val set_enum_default_printer : string -> unit
(** Representative for {!EnumDefault} (default: [Abc]). Strings use their own
    representative, the literal [abc]. *)

val set_tuple_default_printer : string -> unit
(** Representative for {!TupleDefault} (default: [(_, _, _, _, _)]). *)

val set_tag_default_printer : string -> unit
(** Representative for {!TagDefault} (default: [Abd(_)]). *)

(** Default printer for witnesses, using the registered printers.
    A tagged witness without a registered printer is printed as [tag(w)],
    or [tag(_)] for [TagOpaque tag]. Default witnesses are printed as
    the representatives set above. In records, absent fields are omitted,
    and the witness for all other labels is printed as [_ = w]. *)
val pp : Format.formatter -> t -> unit
