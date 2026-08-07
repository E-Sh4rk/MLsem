
type t

val dummy : t
(** Placeholder id, for expressions that do not correspond to any source
    location. It has no location and no notices, and [refresh dummy] is
    [dummy]. *)

val unique : unit -> t
val unique_with_pos : Position.t -> t
val refresh : t -> t
val loc : t -> Position.t
val set_show_notices : t -> bool -> unit
val show_notices : t -> bool
val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit    
