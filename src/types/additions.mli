
open Base
open Tvar

module StrMap : Map.S with type key = String.t

exception TypeDefinitionError of string

(* Construction of types *)

type type_base =
    | TInt of Z.t option * Z.t option | TCharInt of char * char | TSString of string
    | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty | TNil
    | TString | TList | TFloat | TArrowAny | TTupleAny | TTupleN of int | TEnumAny
    | TTagAny | TRecordAny 

type type_regexp = type_expr Sstt.Extensions.Lists.regexp

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

val type_base_to_typ : type_base -> typ

val type_expr_to_typ : type_env -> var_type_env -> type_expr -> typ * var_type_env
val type_exprs_to_typs : type_env -> var_type_env -> type_expr list -> typ list * var_type_env

val define_abstract : type_env -> string -> variance list -> type_env
val define_types : type_env -> var_type_env -> (string * string list * type_expr) list -> type_env
val get_enum : type_env -> string -> enum
val get_tag : type_env -> string -> tag

(* Operations on types *)

val partition : typ list -> typ list
val refine_partition : typ list -> typ -> typ list

val is_test_type : typ -> bool

val branch_type : (typ*typ) list -> typ
val tuple_branch_type : typ list -> typ
val cons_branch_type : (typ*typ) -> typ
val record_branch_type : ((string * (bool * typ)) list * bool) -> typ

(* Simplification and transformations of types *)

val simplify_typ : typ -> typ
val transform_abstract :
    (abstract * (typ list list * typ list list) list ->
                (typ list list * typ list list) list)
        -> typ -> typ

(* Record manipulation *)

val record_any_with : string -> typ
(** [record_any_with l] creates the record type {l = Any ..} *)

val record_any_without : string -> typ
(** [record_any_without l] creates the record type {l =? Empty ..} *)

val remove_field_info : typ -> string -> typ
(** [remove_field_info t label] removes all the information
    about the field label in the record t. *)

(* Operations on type variables *)

val instantiate : Subst.t list -> typ -> typ

val bot_instance : TVarSet.t -> typ -> typ
val top_instance : TVarSet.t -> typ -> typ
