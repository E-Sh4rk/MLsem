open Mlsem_common
open Annot

type severity = Message | Notice | Warning | Error
type msg = { eid: Eid.t ; severity: severity ; title: string ; descr: string option }

(** Sub-expressions reached by {!analyze}. *)
module Visited : sig
    type t
    val empty : t
    val union : t -> t -> t
    val union_many : t list -> t
end

val analyze : Ast.t -> Annot.t -> msg list * Visited.t
(** [analyze e a] reports the notices raised by the annotated expression [e],
    together with the sub-expressions of [e] that [a] covers. The annotation
    must already have been type-checked, since the analysis reads the types
    cached in it.
    @raise Failure if a node of the annotation has no cached type. *)

val get_unreachable : Visited.t -> Ast.t -> msg list
(** [get_unreachable visited e] reports the sub-expressions of [e] that are not
    in [visited], i.e. that no annotation covers.
    A same expression may be analyzed under several annotations (typically once
    per signature of a definition); [visited] must then be the union of the sets
    returned by the corresponding calls to {!analyze}, otherwise the parts that
    only some of them reached are wrongly reported as unreachable. *)
