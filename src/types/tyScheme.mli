open Base
open Tvar

type t
val mk : TVarSet.t -> typ -> t
val mk_mono : typ -> t
val mk_poly : typ -> t
val get : t -> TVarSet.t * typ
val get_fresh : t -> TVarSet.t * typ
val fv : t -> TVarSet.t
val leq : t -> t -> bool
val equiv : t -> t -> bool
val leq_inst : t -> t -> bool
val equiv_inst : t -> t -> bool
val clean : t -> t
val pp : Format.formatter -> t -> unit
val pp_short : Format.formatter -> t -> unit
