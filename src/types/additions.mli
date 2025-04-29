
open Base
open Tvar

module StrMap : Map.S with type key = String.t

exception TypeDefinitionError of string

(* Construction of types *)

type type_base =
    | TInt of Z.t option * Z.t option | TCharInt of char * char | TSString of string
    | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty | TNil
    | TString | TList | TFloat | TArrowAny | TTupleAny | TTupleN of int | TAtomAny
    | TTagAny | TRecordAny 

type type_regexp = type_expr Sstt.Extensions.Lists.regexp

and type_expr =
    | TVar of string | TVarWeak of string
    | TBase of type_base
    | TCustom of string
    | TApp of  string * type_expr list
    | TAtom of string
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
val get_atom : type_env -> string -> atom
val get_tag : type_env -> string -> tag

(* Operations on types *)

val is_test_type : typ -> bool

val branch_type : (typ*typ) list -> typ
val tuple_branch_type : typ list -> typ
val cons_branch_type : (typ*typ) -> typ
val record_branch_type : ((string * (bool * typ)) list * bool) -> typ

(* Simplification of types *)

val simplify_typ : typ -> typ

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

val bot_instance : typ -> typ
val top_instance : typ -> typ

val subtypes_poly : (typ * typ) list -> bool
val supertypes_poly : (typ * typ) list -> bool
val subtype_poly : typ -> typ -> bool
val supertype_poly : typ -> typ -> bool
val is_empty_poly : typ -> bool

val subtype_expand : max_exp:int -> typ -> typ -> Subst.t list option
val subtypes_expand : max_exp:int -> typ -> typ list -> Subst.t list option

val uncorrelate_tvars : typ -> typ
(**
    [uncorrelate_tvars t] uncorrelates the different branches of the
    arrow part of [t] by renaming its polymorphic type variables. It also
    removes redundant branches in order to get a simpler type.
    The type returned by this function is NOT equivalent to [t].
    In particular, it removes negative arrows (for simplification purposes),
    and it may remove some branches and rename some occurrences of type variables.
    Consequently, it should NOT be used to simplify types during the typing of an
    expression: it should ONLY be used to simplify types at top-level, after generalization.
*)