(** Extraction of witnesses (sample inhabitants) from non-empty types. *)

open Sstt

(** A witness: a description of a value inhabiting a type. *)
type t =
  | Other  (** Any value (e.g. of a component that is entirely included) *)
  | Int of Z.t  (** An integer *)
  | Enum of Enum.t  (** An enum constant *)
  | EnumOther  (** An enum constant not mentioned in the type *)
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

(** [of_ty ty] returns a witness of minimal depth for [ty],
    or [None] if [ty] is empty. Type and row variables are ignored. *)
val of_ty : Ty.t -> t option

(** Default printer for witnesses. Absent fields are omitted,
    and the witness for all other labels is printed as [_ = w]. *)
val pp : Format.formatter -> t -> unit
