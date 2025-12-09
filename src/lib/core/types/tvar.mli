open Base

(** @canonical Mlsem_types.Row *)
module Row = Sstt.Row

type kind = KNoInfer | KInfer | KTemporary

module type Var = sig
    type set
    type t

    val all_vars : kind -> set
    val has_kind : kind -> t -> bool
    val kind : t -> kind
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val name : t -> string
    val mk : kind -> string option -> t
    val pp : Format.formatter -> t -> unit
end
module type TVar = sig
    include Var
    val typ : t -> Ty.t
end
module type RVar = sig
    include Var
    val row : t -> Row.t
    val fty : t -> FTy.t
end

module type VarSet = sig
    type var

    include Set.S with type elt=var

    val union_many : t list -> t
    val inter_many : t list -> t
    val pp : Format.formatter -> t -> unit
end

(** @canonical Mlsem_types.TVar *)
module rec TVar : (TVar with type set := TVarSet.t and type t = Sstt.Var.t)

(** @canonical Mlsem_types.TVarSet *)
and TVarSet : (VarSet with type var := TVar.t and type t = Sstt.VarSet.t)

(** @canonical Mlsem_types.RVar *)
module rec RVar : (RVar with type set := RVarSet.t and type t = Sstt.RowVar.t)

(** @canonical Mlsem_types.RVarSet *)
and RVarSet : (VarSet with type var := RVar.t and type t = Sstt.RowVarSet.t)

(** @canonical Mlsem_types.MVarSet *)
module MVarSet = Sstt.MixVarSet

(** @canonical Mlsem_types.Subst *)
module Subst = Sstt.Subst

(** @canonical Mlsem_types.TVOp *)
module TVOp : sig
    val vars : Ty.t -> MVarSet.t
    val vars' : Ty.t list -> MVarSet.t
    val top_vars : Ty.t -> MVarSet.t
    val strict_vars : Ty.t -> MVarSet.t
    val vars_of_kind : kind -> Ty.t -> MVarSet.t
    val polarity1 : TVar.t -> Ty.t -> [ `Both | `Neg | `Pos | `None ]
    val polarity2 : RVar.t -> Ty.t -> [ `Both | `Neg | `Pos | `None ]
    val polarity1' : TVar.t -> Ty.t list -> [ `Both | `Neg | `Pos | `None ]
    val polarity2' : RVar.t -> Ty.t list -> [ `Both | `Neg | `Pos | `None ]
    val vars_with_polarity1 : Ty.t -> (TVar.t * [ `Both | `Neg | `Pos ]) list
    val vars_with_polarity2 : Ty.t -> (RVar.t * [ `Both | `Neg | `Pos ]) list
    val vars_with_polarity1' : Ty.t list -> (TVar.t * [ `Both | `Neg | `Pos ]) list
    val vars_with_polarity2' : Ty.t list -> (RVar.t * [ `Both | `Neg | `Pos ]) list
    val is_ground_typ : Ty.t -> bool
    val refresh : kind:kind -> MVarSet.t -> Subst.t
    val shorten_names : MVarSet.t -> Subst.t
    val pp_typ_short : Format.formatter -> Ty.t -> unit
    val pp_typ_subst : Subst.t -> Format.formatter -> Ty.t -> unit

    (** [clean p n mono t] substitutes in [t]
        all variables not in [mono] and only occurring positively by [p], and
        all variables not in [mono] and only occurring negatively by [n] *)
    val clean : pos1:Ty.t -> neg1:Ty.t -> pos2:Row.t -> neg2:Row.t -> MVarSet.t -> Ty.t -> Ty.t
    val clean_subst : pos1:Ty.t -> neg1:Ty.t -> pos2:Row.t -> neg2:Row.t -> MVarSet.t -> Ty.t -> Subst.t
    val clean' : pos1:Ty.t -> neg1:Ty.t -> pos2:Row.t -> neg2:Row.t -> MVarSet.t -> Ty.t list -> Ty.t list
    val clean_subst' : pos1:Ty.t -> neg1:Ty.t -> pos2:Row.t -> neg2:Row.t -> MVarSet.t -> Ty.t list -> Subst.t

    val bot_instance : MVarSet.t -> Ty.t -> Ty.t
    val top_instance : MVarSet.t -> Ty.t -> Ty.t

    val tallying : MVarSet.t -> (Ty.t * Ty.t) list -> Subst.t list
    val decompose : MVarSet.t -> Subst.t -> Subst.t -> Subst.t list

    val factorize : TVarSet.t * TVarSet.t -> Ty.t -> Ty.t * Ty.t

    (** Operations on row and field variables *)

    type field_ctx
    val get_field_ctx : RVarSet.t -> Ty.t list -> field_ctx
    val decorrelate_fields : field_ctx -> Ty.t -> Ty.t
    val recombine_fields : field_ctx -> Ty.t -> Ty.t
    val recombine_fields' : field_ctx -> Subst.t -> Subst.t
    val fvars_associated_with : field_ctx -> RVar.t -> RVarSet.t
    val rvar_associated_with : field_ctx -> RVar.t -> (RVar.t * string) option
    val tallying_fields : MVarSet.t -> (Ty.t * Ty.t) list -> Subst.t list
end