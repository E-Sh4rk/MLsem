open Tvar
open Gradual

module TyScheme : sig
    type t
    val mk : TVarSet.t -> GTy.t -> t
    val mk_poly_except : TVarSet.t -> GTy.t -> t
    val mk_mono : GTy.t -> t
    val mk_poly : GTy.t -> t
    val get : t -> TVarSet.t * GTy.t
    val get_fresh : t -> TVarSet.t * GTy.t
    val fv : t -> TVarSet.t
    val substitute : Subst.t -> t -> t
    val leq : t -> t -> bool
    val equiv : t -> t -> bool
    val bot_instance : t -> t
    val top_instance : t -> t
    val normalize : t -> t
    val simplify : t -> t
    val norm_and_simpl : t -> t
    val pp : Format.formatter -> t -> unit
    val pp_short : Format.formatter -> t -> unit
end
