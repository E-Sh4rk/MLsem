open Base
open Tvar

(** @canonical Mlsem_types.TyExpr *)
module TyExpr : sig
    type base =
        | TInt of Z.t option * Z.t option | TCharInt of char * char | TSString of string
        | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty | TNil
        | TString | TList | TFloat | TArrowAny | TTupleAny | TTupleN of int | TEnumAny
        | TTagAny | TRecordAny 

    type 'ext regexp =
        | Epsilon | Symbol of 'ext t
        | Union of 'ext regexp list | Concat of 'ext regexp list
        | Star of 'ext regexp | Plus of 'ext regexp | Option of 'ext regexp

    and 'ext t =
        (* Type constructors *)
        | TVar of kind * string
        | TRowVar of kind * string
        | TBase of base
        | TApp of  string * 'ext t list
        | TEnum of string
        | TTag of string * 'ext t
        | TTuple of 'ext t list
        | TRecord of (string * 'ext t) list * 'ext t
        | TSList of 'ext regexp
        | TCons of 'ext t * 'ext t
        | TArrow of 'ext t * 'ext t
        | TOption of 'ext t
        (* Type connectives *)
        | TCustom of string
        | TCup of 'ext t * 'ext t
        | TCap of 'ext t * 'ext t
        | TDiff of 'ext t * 'ext t
        | TNeg of 'ext t
        | TWhere of 'ext t * (string * string list * 'ext t) list
        (* Type operators (may inspect their parameters!) *)
        | TRecUpd of 'ext t * (string * 'ext t) list
        (* Custom extensions *)
        | TExt of 'ext
end

(** @canonical Mlsem_types.Builder' *)
module Builder' : sig
    module type B = sig
        type ext

        exception TypeDefinitionError of string

        type type_base = TyExpr.base
        type type_regexp = ext TyExpr.regexp
        type type_expr = ext TyExpr.t

        type type_env
        type var_type_env
        val empty_tenv : type_env
        val empty_vtenv : var_type_env

        type benv = { tenv:type_env ; vtenv:var_type_env }
        val empty_benv : benv

        val type_base_to_typ : type_base -> Ty.t

        val type_expr_to_typ : benv -> type_expr -> Ty.t * benv
        val type_exprs_to_typs : benv -> type_expr list -> Ty.t list * benv

        val define_abstract : benv -> string -> int -> benv
        val define_aliases : benv -> (string * string list * type_expr) list -> benv
        val get_enum : benv -> string -> Enum.t * benv
        val get_tag : benv -> string -> Tag.t * benv

        val is_test_type : Ty.t -> bool
    end

    module type Ext = sig
        type t
        val to_typ : (t TyExpr.t -> Ty.t) -> t -> Ty.t
    end

    module Make(E:Ext) : B with type ext = E.t
end

(** @canonical Mlsem_types.empty *)
type empty = |

(** @canonical Mlsem_types.Builder *)
module Builder : Builder'.B with type ext = empty
