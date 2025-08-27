open Base
open Tvar

(** @canonical Mlsem.Types.TyExpr *)
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
        | TVar of TVar.kind * string
        | TBase of base
        | TCustom of string
        | TApp of  string * 'ext t list
        | TEnum of string
        | TTag of string * 'ext t
        | TTuple of 'ext t list
        | TRecord of bool * (string * 'ext t * bool) list
        | TSList of 'ext regexp
        | TCons of 'ext t * 'ext t
        | TArrow of 'ext t * 'ext t
        | TCup of 'ext t * 'ext t
        | TCap of 'ext t * 'ext t
        | TDiff of 'ext t * 'ext t
        | TNeg of 'ext t
        | TWhere of 'ext t * (string * string list * 'ext t) list
        | TExt of 'ext
end

(** @canonical Mlsem.Types.Builder' *)
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

        val type_base_to_typ : type_base -> Ty.t

        val type_expr_to_typ : type_env -> var_type_env -> type_expr -> Ty.t * var_type_env
        val type_exprs_to_typs : type_env -> var_type_env -> type_expr list -> Ty.t list * var_type_env

        val define_abstract : type_env -> string -> Abstract.variance list -> type_env
        val define_aliases : type_env -> var_type_env -> (string * string list * type_expr) list -> type_env
        val get_enum : type_env -> string -> Enum.t
        val get_tag : type_env -> string -> Tag.t

        val is_test_type : Ty.t -> bool
    end

    module type Ext = sig
        type t
        val to_typ : (t TyExpr.t -> Ty.t) -> t -> Ty.t
    end

    module Make(E:Ext) : B with type ext = E.t
end

(** @canonical Mlsem.Types.empty *)
type empty = |

(** @canonical Mlsem.Types.Builder *)
module Builder : Builder'.B with type ext = empty
