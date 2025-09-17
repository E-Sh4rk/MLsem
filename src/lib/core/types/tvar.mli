open Base

module type TVar = sig
    type set
    type t = Sstt.Var.t
    type kind = KNoInfer | KInfer | KTemporary

    val all_vars : kind -> set
    val has_kind : kind -> t -> bool
    val kind : t -> kind
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val name : t -> string

    val mk : kind -> string option -> t
    val typ : t -> Ty.t

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

(** @canonical Mlsem_types.TVar *)
module rec TVar : (TVar with type set := TVarSet.t)

(** @canonical Mlsem_types.TVarSet *)
and TVarSet : (TVarSet with type var := TVar.t)

(** @canonical Mlsem_types.Subst *)
module Subst : sig
    type t
    val construct : (TVar.t * Ty.t) list -> t
    val identity : t
    val is_identity : t -> bool
    val dom : t -> TVarSet.t
    val vars : t -> TVarSet.t
    val mem : t -> TVar.t -> bool
    val rm: TVar.t -> t -> t
    val find : t -> TVar.t -> Ty.t
    val equiv : t -> t -> bool
    val apply : t -> Ty.t -> Ty.t
    val destruct : t -> (TVar.t * Ty.t) list
    val compose : t -> t -> t
    val compose_restr : t -> t -> t
    val combine : t -> t -> t
    val restrict : t -> TVarSet.t -> t
    val remove : t -> TVarSet.t -> t
    val split : t -> TVarSet.t -> t * t
    val pp : Format.formatter -> t -> unit
end

(** @canonical Mlsem_types.TVOp *)
module TVOp : sig
    val vars : Ty.t -> TVarSet.t
    val vars' : Ty.t list -> TVarSet.t
    val vars_of_kind : TVar.kind -> Ty.t -> TVarSet.t
    val top_vars : Ty.t -> TVarSet.t
    val polarity : TVar.t -> Ty.t -> [ `Both | `Neg | `Pos | `None ]
    val polarity' : TVar.t -> Ty.t list -> [ `Both | `Neg | `Pos | `None ]
    val vars_with_polarity : Ty.t -> (TVar.t * [ `Both | `Neg | `Pos ]) list
    val vars_with_polarity' : Ty.t list -> (TVar.t * [ `Both | `Neg | `Pos ]) list
    val check_var : Ty.t -> [ `Not_var | `Pos of TVar.t | `Neg of TVar.t ]
    val is_ground_typ : Ty.t -> bool
    val refresh : kind:TVar.kind -> TVarSet.t -> Subst.t
    val shorten_names : TVarSet.t -> Subst.t
    val pp_typ_short : Format.formatter -> Ty.t -> unit

    (** [clean p n mono t] substitutes in [t]
        all type variables not in [mono] and only occurring positively by [p], and
        all type variables not in [mono] and only occurring negatively by [n] *)
    val clean : pos:Ty.t -> neg:Ty.t -> TVarSet.t -> Ty.t -> Ty.t
    val clean_subst : pos:Ty.t -> neg:Ty.t -> TVarSet.t -> Ty.t -> Subst.t
    val clean' : pos:Ty.t -> neg:Ty.t -> TVarSet.t -> Ty.t list -> Ty.t list
    val clean_subst' : pos:Ty.t -> neg:Ty.t -> TVarSet.t -> Ty.t list -> Subst.t

    val bot_instance : TVarSet.t -> Ty.t -> Ty.t
    val top_instance : TVarSet.t -> Ty.t -> Ty.t

    val tallying : TVarSet.t -> (Ty.t * Ty.t) list -> Subst.t list
    val tallying_with_prio : TVarSet.t -> (TVar.t list) -> (Ty.t * Ty.t) list -> Subst.t list

    val factorize : TVarSet.t * TVarSet.t -> Ty.t -> Ty.t * Ty.t
end