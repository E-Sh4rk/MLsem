
type t = int
val dummy : t
val unique : unit -> t
val unique_with_pos : Position.t -> t
val refresh : t -> t
val loc : t -> Position.t
val pp : Format.formatter -> t -> unit    
