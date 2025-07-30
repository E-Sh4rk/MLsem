open Base

module StrMap : Map.S with type key = String.t

exception TypeDefinitionError of string

(* Construction of types *)

type type_base =
    | TInt of Z.t option * Z.t option | TCharInt of char * char | TSString of string
    | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty | TNil
    | TString | TList | TFloat | TArrowAny | TTupleAny | TTupleN of int | TEnumAny
    | TTagAny | TRecordAny 

type type_regexp =
    | Epsilon | Symbol of type_expr 
    | Union of type_regexp list | Concat of type_regexp list
    | Star of type_regexp | Plus of type_regexp | Option of type_regexp

and type_expr =
    | TVar of string | TVarWeak of string
    | TBase of type_base
    | TCustom of string
    | TApp of  string * type_expr list
    | TEnum of string
    | TTag of string * type_expr
    | TTuple of type_expr list
    | TRecord of bool * (string * type_expr * bool) list
    | TSList of type_regexp
    | TCons of type_expr * type_expr
    | TArrow of type_expr * type_expr
    | TCup of type_expr * type_expr
    | TCap of type_expr * type_expr
    | TDiff of type_expr * type_expr
    | TNeg of type_expr
    | TWhere of type_expr * (string * string list * type_expr) list

type type_env
type var_type_env

val empty_tenv : type_env
val empty_vtenv : var_type_env

val type_base_to_typ : type_base -> Ty.t

val type_expr_to_typ : type_env -> var_type_env -> type_expr -> Ty.t * var_type_env
val type_exprs_to_typs : type_env -> var_type_env -> type_expr list -> Ty.t list * var_type_env

val define_abstract : type_env -> string -> Abstract.variance list -> type_env
val define_types : type_env -> var_type_env -> (string * string list * type_expr) list -> type_env
val get_enum : type_env -> string -> Enum.t
val get_tag : type_env -> string -> Tag.t

val is_test_type : Ty.t -> bool
