
type typ = Sstt.Ty.t

val register : string -> typ -> unit
val pp_typ : Format.formatter -> typ -> unit
val pp_subst : Format.formatter -> Sstt.Subst.t -> unit

val any : typ
val empty : typ

val true_typ : typ
val false_typ : typ
val bool_typ : typ
val int_typ : typ
val float_typ : typ
val char_typ : typ
val unit_typ : typ
val string_typ : typ
val interval : Z.t option -> Z.t option -> typ
val char_interval : char -> char -> typ
val single_string : string -> typ

val neg : typ -> typ
val cup : typ -> typ -> typ
val cap : typ -> typ -> typ
val diff : typ -> typ -> typ
val conj : typ list -> typ
val disj : typ list -> typ

type atom
val pp_atom : Format.formatter -> atom -> unit
val compare_atom : atom -> atom -> int
val define_atom : string -> atom
val mk_atom : atom -> typ
val atom_any : typ

type tag
val pp_tag : Format.formatter -> tag -> unit
val compare_tag : tag -> tag -> int
val define_tag : string -> tag
val mk_tag : tag -> typ -> typ
val destruct_tag : tag -> typ -> typ
val tag_any : typ

type variance = Cov | Cav | Inv
type abstract
val define_abstract : string -> variance list -> abstract
val mk_abstract : abstract -> typ list -> typ
val mk_abstract_any : abstract -> typ

val mk_tuple : typ list -> typ
val tuple_any : typ
val tuple_n : int -> typ
val pi : int -> int -> typ -> typ
val tuple_dnf : int -> typ -> typ list list

val nil_typ : typ
val list_typ : typ
val non_empty_list_typ : typ
val mk_cons : typ -> typ -> typ
val cons_dnf : typ -> (typ * typ) list
val destruct_list : typ -> typ * typ

val to_label : string -> Sstt.Label.t
val from_label : Sstt.Label.t -> string
val mk_record : bool (* is_open *) -> (string * (bool * typ)) list -> typ
val record_dnf : typ -> ((string * (bool * typ)) list * bool) list
val record_any : typ
val empty_closed_record : typ
val get_field : typ -> string -> typ
val merge_records : typ -> typ -> typ
val remove_field : typ -> string -> typ

val mk_arrow : typ -> typ -> typ
val arrow_any : typ
val domain : typ -> typ
val apply : typ -> typ -> typ
val dnf : typ -> (typ * typ) list list

val is_empty : typ -> bool
val non_empty: typ -> bool
val subtype  : typ -> typ -> bool
val disjoint : typ -> typ -> bool
val equiv : typ -> typ -> bool
