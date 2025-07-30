open Common
open Types
open Annot

val domain_of_proj : Ast.projection -> Ty.t -> Ty.t
val proj : Ast.projection -> Ty.t -> Ty.t

val domains_of_construct : Ast.constructor -> Ty.t list
val construct : Ast.constructor -> Ty.t list -> Ty.t

type error = { eid: Eid.t ; title: string ; descr: string option }
exception Untypeable of error

val typeof : Env.t -> Annot.t -> Ast.t -> GTy.t
val generalize : e:Ast.t -> Env.t -> GTy.t -> TyScheme.t
val typeof_def : Env.t -> Annot.t -> Ast.t -> TyScheme.t
