open Base
open Tvar

module GTy : sig
    type t
    val empty : t
    val any : t
    val mk: typ -> t
    val mk_gradual: typ -> typ -> t
    val lb: t -> typ
    val ub: t -> typ
    val destruct : t -> typ * typ
    val cup: t -> t -> t
    val cap: t -> t -> t
    val disj : t list -> t
    val conj : t list -> t
    val neg : t -> t

    val fv : t -> TVarSet.t
    val substitute : Subst.t -> t -> t
    (* Mapping functions below assume the operation is monotonic *)
    val map : (typ -> typ) -> t -> t
    val map2 : (typ -> typ -> typ) -> t -> t -> t
    val op : (typ -> typ option) -> t -> t option
    val op2 : (typ -> typ -> typ option) -> t -> t -> t option

    val is_empty : t -> bool
    val is_any : t -> bool
    val leq : t -> t -> bool
    val equiv : t -> t -> bool

    val simplify : t -> t
    val normalize : t -> t

    val pp : Format.formatter -> t -> unit
end
