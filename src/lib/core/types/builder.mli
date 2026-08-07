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
        | TDyn
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
        | TTagProj of t * string
end

(** @canonical Mlsem_types.Builder *)
module Builder : sig
    exception TypeDefinitionError of string
    (** Raised by every function below that elaborates a type expression, for
        any way in which the expression can be ill-formed: an undefined or
        misapplied type name, a non-regular recursive definition, a type
        operator applied to something it cannot inspect, a [dyn] where none is
        allowed. The payload is a message meant for the user. *)

    type type_env
    type var_type_env
    val empty_tenv : type_env
    val empty_vtenv : var_type_env

    type benv = { tenv:type_env ; vtenv:var_type_env }
    val empty_benv : benv

    (* User-defined type names known in the environment (aliases, enums, tags,
       abstract types), sorted and de-duplicated. *)
    val type_names : benv -> string list

    val type_base_to_typ : TyExpr.base -> Ty.t

    val type_expr_to_typ : ?allow_gradual:bool -> benv -> TyExpr.t -> Ty.t * benv
    (** Elaborates a type expression into a static type. With
        [~allow_gradual:true] the expression may contain [dyn] occurrences, as
        long as none of them is in an invariant position.
        @raise TypeDefinitionError if the expression is ill-formed. *)

    val type_exprs_to_typs : ?allow_gradual:bool -> benv -> TyExpr.t list -> Ty.t list * benv
    (** @raise TypeDefinitionError if one of the expressions is ill-formed. *)

    val type_expr_to_gty : benv -> TyExpr.t -> GTy.t * benv
    (** Elaborates a type expression into a gradual type, [dyn] occurrences
        becoming the bounds of the interval.
        @raise TypeDefinitionError if the expression is ill-formed, in
        particular if a [dyn] occurs in an invariant position. *)

    val type_exprs_to_gtys : benv -> TyExpr.t list -> GTy.t list * benv
    (** @raise TypeDefinitionError if one of the expressions is ill-formed. *)

    val define_abstract : benv -> string -> int -> benv
    (** @raise TypeDefinitionError if the abstract type is already defined. *)

    val define_aliases : benv -> (string * string list * TyExpr.t) list -> benv
    (** Defines a group of mutually recursive type aliases.
        @raise TypeDefinitionError if one of the definitions is ill-formed. *)

    val get_enum : benv -> string -> Enum.t * benv
    (** The enum of that name, defining it if it does not exist yet. *)

    val get_tag : benv -> string -> Tag.t * benv
    (** The tag of that name, defining it if it does not exist yet. *)

    val is_test_type : Ty.t -> bool
end
