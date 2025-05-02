
module type TVar = sig
    type set
    type t = Sstt.Var.t

    val user_vars : unit -> set
    val from_user : t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val name : t -> string
    val display_name : t -> string

    val mk : ?user:bool -> string option -> t
    val typ : t -> Base.typ

    val pp : Format.formatter -> t -> unit
end

module type TVarSet = sig
    type var
    type t

    val empty : t
    val construct : var list -> t
    val is_empty : t -> bool
    val mem : t -> var -> bool
    val filter : (var -> bool) -> t -> t
    val union : t -> t -> t
    val union_many : t list -> t
    val add : var -> t -> t
    val inter : t -> t -> t
    val inter_many : t list -> t
    val diff : t -> t -> t
    val rm : var -> t -> t
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val destruct : t -> var list
    val pp : Format.formatter -> t -> unit
end

module rec TVar : (TVar with type set := TVarSet.t)
and TVarSet : (TVarSet with type var := TVar.t)

module Subst : sig
    type t
    val construct : (TVar.t * Base.typ) list -> t
    val identity : t
    val is_identity : t -> bool
    val dom : t -> TVarSet.t
    val vars : t -> TVarSet.t
    val mem : t -> TVar.t -> bool
    val rm: TVar.t -> t -> t
    val find : t -> TVar.t -> Base.typ
    val equiv : t -> t -> bool
    val apply : t -> Base.typ -> Base.typ
    val destruct : t -> (TVar.t * Base.typ) list
    val compose : t -> t -> t
    val compose_restr : t -> t -> t
    val combine : t -> t -> t
    val restrict : t -> TVarSet.t -> t
    val remove : t -> TVarSet.t -> t
    val split : t -> TVarSet.t -> t * t
    val is_renaming : t -> bool
    (* val inverse_renaming : t -> t *)
    val pp : Format.formatter -> t -> unit
end

val vars : Base.typ -> TVarSet.t
val vars' : Base.typ list -> TVarSet.t
val vars_user : Base.typ -> TVarSet.t
val vars_internal : Base.typ -> TVarSet.t
val top_vars : Base.typ -> TVarSet.t
val polarity : TVar.t -> Base.typ -> [ `Both | `Neg | `Pos | `None ]
val polarity' : TVar.t -> Base.typ list -> [ `Both | `Neg | `Pos | `None ]
val vars_with_polarity : Base.typ -> (TVar.t * [ `Both | `Neg | `Pos ]) list
val vars_with_polarity' : Base.typ list -> (TVar.t * [ `Both | `Neg | `Pos ]) list
val check_var : Base.typ -> [ `Not_var | `Pos of TVar.t | `Neg of TVar.t ]
val is_ground_typ : Base.typ -> bool
val refresh : TVarSet.t -> Subst.t
val shorten_names : TVarSet.t -> Subst.t
val pp_typ_short : Format.formatter -> Base.typ -> unit

(* Operations on types *)

(** [clean p n mono t] substitutes in [t]
    all type variables not in [mono] and only occurring positively by [p], and
    all type variables not in [mono] and only occurring negatively by [n] *)
val clean : pos:Base.typ -> neg:Base.typ -> TVarSet.t -> Base.typ -> Base.typ
val clean_subst : pos:Base.typ -> neg:Base.typ -> TVarSet.t -> Base.typ -> Subst.t
val clean' : pos:Base.typ -> neg:Base.typ -> TVarSet.t -> Base.typ list -> Base.typ list
val clean_subst' : pos:Base.typ -> neg:Base.typ -> TVarSet.t -> Base.typ list -> Subst.t

val test_tallying : TVarSet.t -> (Base.typ * Base.typ) list -> bool
val tallying : TVarSet.t -> (Base.typ * Base.typ) list -> Subst.t list
val tallying_with_prio : TVarSet.t -> (TVar.t list) -> (Base.typ * Base.typ) list -> Subst.t list

val factorize : TVarSet.t * TVarSet.t -> Base.typ -> Base.typ * Base.typ
