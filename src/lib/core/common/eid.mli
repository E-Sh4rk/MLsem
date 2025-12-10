
type t
val dummy : t
val unique : unit -> t
val unique_with_pos : Position.t -> t
val refresh : t -> t
val loc : t -> Position.t
val show_notices : t -> bool
val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit    
