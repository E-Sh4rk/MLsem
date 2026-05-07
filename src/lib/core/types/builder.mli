open Base
open Tvar

(** @canonical Mlsem_types.TyExpr *)
module TyExpr : sig
    type base =
        | TInt of Z.t option * Z.t option | TCharInt of char * char | TSString of string
        | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty | TNil
        | TString | TList | TFloat | TArrowAny | TTupleAny | TTupleN of int | TEnumAny
        | TTagAny | TRecordAny 

    type regexp =
        | Epsilon | Symbol of t
        | Union of regexp list | Concat of regexp list
        | Star of regexp | Plus of regexp | Option of regexp

    and t =
        (* Type constructors *)
        | TVar of kind * string
        | TRowVar of kind * string
        | TBase of base
        | TApp of  string * t list
        | TEnum of string
        | TTag of string * t
        | TTuple of t list
        | TRecord of (string * t) list * t
        | TSList of regexp
        | TCons of t * t
        | TArrow of t * t
        | TOption of t
        (* Type connectives *)
        | TCustom of string
        | TCup of t * t
        | TCap of t * t
        | TDiff of t * t
        | TNeg of t
        | TWhere of t * (string * string list * t) list
        (* Type operators (may inspect their parameters!) *)
        | TRecUpd of t * (string * t) list
        | TRecProj of t * string
end

(** @canonical Mlsem_types.Builder *)
module Builder : sig
    exception TypeDefinitionError of string

    type type_env
    type var_type_env
    val empty_tenv : type_env
    val empty_vtenv : var_type_env

    type benv = { tenv:type_env ; vtenv:var_type_env }
    val empty_benv : benv

    val type_base_to_typ : TyExpr.base -> Ty.t

    val type_expr_to_typ : benv -> TyExpr.t -> Ty.t * benv
    val type_exprs_to_typs : benv -> TyExpr.t list -> Ty.t list * benv

    val define_abstract : benv -> string -> int -> benv
    val define_aliases : benv -> (string * string list * TyExpr.t) list -> benv
    val get_enum : benv -> string -> Enum.t * benv
    val get_tag : benv -> string -> Tag.t * benv

    val is_test_type : Ty.t -> bool
end
