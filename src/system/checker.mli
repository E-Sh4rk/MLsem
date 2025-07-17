open Types.Base
open Types
open Annot
open Env

val domain_of_proj : Ast.projection -> typ -> typ
val proj : Ast.projection -> typ -> typ

val domains_of_construct : Ast.constructor -> typ list
val construct : Ast.constructor -> typ list -> typ

type error = { eid: Eid.t ; title: string ; descr: string option }
exception Untypeable of error

val typeof : Env.t -> Annot.t -> Ast.t -> typ
val generalize : e:Ast.t -> Env.t -> typ -> TyScheme.t
val typeof_def : Env.t -> Annot.t -> Ast.t -> TyScheme.t
