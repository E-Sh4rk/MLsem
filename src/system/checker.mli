open Common
open Types.Base
open Types
open Annot
open Gradual

val domain_of_proj : Ast.projection -> typ -> typ
val proj : Ast.projection -> typ -> typ

val domains_of_construct : Ast.constructor -> typ list
val construct : Ast.constructor -> typ list -> typ

type error = { eid: Eid.t ; title: string ; descr: string option }
exception Untypeable of error

val typeof : Env.t -> Annot.t -> Ast.t -> GTy.t
val generalize : e:Ast.t -> Env.t -> GTy.t -> TyScheme.t
val typeof_def : Env.t -> Annot.t -> Ast.t -> TyScheme.t
