open Types.Base
open Types.Additions
open Annotations
open Parsing.Variable

val domain_of_proj : Parsing.Ast.projection -> typ -> typ
val proj : Parsing.Ast.projection -> typ -> typ

(** This module implements the algorithmic type system. *)

exception Untypeable of Position.t list * string

(** The functions below will raise [Untypeable] if the annotation
    is not valid for the expression. *)

val typeof : type_env -> Env.t -> FullAnnot.t_cached -> Msc.e -> typ
val typeof_a : Variable.t -> type_env -> Env.t -> FullAnnot.a_cached -> Msc.a -> typ

(** The functions below will raise [Assert_failure] if the annotation
    is not valid for the expression. *)

val typeof_nofail : type_env -> Env.t -> FullAnnot.t_cached -> Msc.e -> typ
val typeof_a_nofail : Variable.t -> type_env -> Env.t -> FullAnnot.a_cached -> Msc.a -> typ
