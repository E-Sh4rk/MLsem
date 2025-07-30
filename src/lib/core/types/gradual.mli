open Base
open Tvar

module GTy : sig
    type t
    val empty : t
    val any : t
    val dyn : t
    val mk: Ty.t -> t
    val mk_gradual: Ty.t -> Ty.t -> t
    val lb: t -> Ty.t
    val ub: t -> Ty.t
    val destruct : t -> Ty.t * Ty.t
    val cup: t -> t -> t
    val cap: t -> t -> t
    val disj : t list -> t
    val conj : t list -> t
    val neg : t -> t

    val fv : t -> TVarSet.t
    val substitute : Subst.t -> t -> t
    (* Mapping functions below assume the operation is monotonic *)
    val map : (Ty.t -> Ty.t) -> t -> t
    val map2 : (Ty.t -> Ty.t -> Ty.t) -> t -> t -> t
    val mapl : (Ty.t list -> Ty.t) -> t list -> t
    val op : (Ty.t -> bool) -> (Ty.t -> Ty.t) -> t -> t option
    val op2 : (Ty.t -> Ty.t -> bool) -> (Ty.t -> Ty.t -> Ty.t) -> t -> t -> t option
    val opl : (Ty.t list -> bool) -> (Ty.t list -> Ty.t) -> t list -> t option

    val is_empty : t -> bool
    val is_any : t -> bool
    val leq : t -> t -> bool
    val equiv : t -> t -> bool

    val simplify : t -> t
    val normalize : t -> t

    val pp : Format.formatter -> t -> unit
end
