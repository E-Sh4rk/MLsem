
(** @canonical Mlsem.Types.Ty *)
module Ty : sig
    type t = Sstt.Ty.t

    val register : string -> t -> unit
    val add_printer_param : Sstt.Printer.params -> unit
    val printer_params : unit -> Sstt.Printer.params
    val pp : Format.formatter -> t -> unit

    val any : t
    val empty : t

    val tt : t
    val ff : t
    val bool : t
    val int : t
    val float : t
    val char : t
    val unit : t
    val string : t

    val interval : Z.t option -> Z.t option -> t
    val char_interval : char -> char -> t
    val string_lit : string -> t

    val neg : t -> t
    val cup : t -> t -> t
    val cap : t -> t -> t
    val diff : t -> t -> t
    val conj : t list -> t
    val disj : t list -> t

    val is_empty : t -> bool
    val is_any : t -> bool
    val non_empty: t -> bool
    val non_any : t -> bool
    val leq  : t -> t -> bool
    val disjoint : t -> t -> bool
    val equiv : t -> t -> bool

    val normalize : t -> t
    val simplify : t -> t
end

(** @canonical Mlsem.Types.Enum *)
module Enum : sig
    type t
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
    val define : string -> t
    val any : Ty.t
    val typ : t -> Ty.t
end

(** @canonical Mlsem.Types.Tag *)
module Tag : sig
    type t
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
    val define : string -> t
    val any : Ty.t
    val mk : t -> Ty.t -> Ty.t
    val proj : t -> Ty.t -> Ty.t
end

(** @canonical Mlsem.Types.Abstract *)
module Abstract : sig
    type t
    val define : string -> int -> t
    val arity : t -> int
    val any : t -> Ty.t
    val mk : t -> Ty.t list -> Ty.t
    val dnf : t -> Ty.t -> (Ty.t list) list list
    val transform :
        (t * (Ty.t list list * Ty.t list list) list
          -> (Ty.t list list * Ty.t list list) list)
        -> Ty.t -> Ty.t
end

(** @canonical Mlsem.Types.Tuple *)
module Tuple : sig
    val any : Ty.t
    val any_n : int -> Ty.t
    val mk : Ty.t list -> Ty.t
    val proj : int -> int -> Ty.t -> Ty.t
    val dnf : int -> Ty.t -> Ty.t list list
    val of_dnf : int -> Ty.t list list -> Ty.t
    val decompose : Ty.t -> (int * Ty.t list list) list * bool
    val recompose : (int * Ty.t list list) list * bool -> Ty.t
end

(** @canonical Mlsem.Types.Lst *)
module Lst : sig
    val nil : Ty.t
    val any : Ty.t
    val any_non_empty : Ty.t
    val cons : Ty.t -> Ty.t -> Ty.t
    val dnf : Ty.t -> (Ty.t * Ty.t) list
    val proj : Ty.t -> Ty.t * Ty.t
end

(** @canonical Mlsem.Types.Record *)
module Record : sig
    val mk : bool (* is_open *) -> (string * (bool * Ty.t)) list -> Ty.t
    val any : Ty.t
    val empty_closed : Ty.t
    val any_with : string -> Ty.t
    val any_without : string -> Ty.t
    val dnf : Ty.t -> ((string * (bool * Ty.t)) list * bool) list
    val of_dnf : ((string * (bool * Ty.t)) list * bool) list -> Ty.t
    val proj : Ty.t -> string -> Ty.t
    val merge : Ty.t -> Ty.t -> Ty.t
    val remove_field : Ty.t -> string -> Ty.t

    val from_label : Sstt.Label.t -> string
    val to_label : string -> Sstt.Label.t
end

(** @canonical Mlsem.Types.Arrow *)
module Arrow : sig
    val mk : Ty.t -> Ty.t -> Ty.t
    val any : Ty.t
    val domain : Ty.t -> Ty.t
    val apply : Ty.t -> Ty.t -> Ty.t
    val dnf : Ty.t -> (Ty.t * Ty.t) list list
    val of_dnf : (Ty.t * Ty.t) list list -> Ty.t
end
