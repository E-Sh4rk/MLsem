open Mlsem_common
open Mlsem_types
open Annot

type severity = Message | Notice | Warning | Error

(** Diagnostics reported by {!analyze} and {!get_unreachable}.

    A diagnostic only records the data that explains it; its textual
    rendering is left to the client, {!Diagnostic.pp} being a default printer. *)
module Diagnostic : sig
    type kind =
    | Unchecked of { check: Ast.check ; expected: GTy.t ; given: GTy.t }
    (** A coercion whose expression type [given] is not a subtype of the type
        [expected], and that is not checked at runtime: [check] is either
        [CheckStatic] (only the static part is checked) or [NoCheck]. *)
    | Unreachable
    (** The expression is not reached by any annotation. *)

    type t = {
        eid: Eid.t ;  (** The expression concerned by the diagnostic *)
        kind: kind
    }

    val severity : t -> severity

    (** {1 Default printers} *)

    val title : t -> string
    (** A short, one-line summary of the diagnostic. *)

    val pp_descr : Format.formatter -> t -> unit
    (** Prints the details of the diagnostic, or nothing if there are none. *)

    val descr : t -> string option
    (** The output of {!pp_descr}, or [None] if it is empty. *)

    val pp : Format.formatter -> t -> unit
    (** Prints the {!title} followed by the {!descr}, if any. *)
end

(** Sub-expressions reached by {!analyze}. *)
module Visited : sig
    type t
    val empty : t
    val union : t -> t -> t
    val union_many : t list -> t
end

val analyze : Ast.t -> Annot.t -> Diagnostic.t list * Visited.t
(** [analyze e a] reports the diagnostics raised by the annotated expression [e],
    together with the sub-expressions of [e] that [a] covers. The annotation
    must already have been type-checked, since the analysis reads the types
    cached in it.
    @raise Failure if a node of the annotation has no cached type. *)

val get_unreachable : Visited.t -> Ast.t -> Diagnostic.t list
(** [get_unreachable visited e] reports the sub-expressions of [e] that are not
    in [visited], i.e. that no annotation covers (as {!Diagnostic.Unreachable}).
    A same expression may be analyzed under several annotations (typically once
    per signature of a definition); [visited] must then be the union of the sets
    returned by the corresponding calls to {!analyze}, otherwise the parts that
    only some of them reached are wrongly reported as unreachable. *)
