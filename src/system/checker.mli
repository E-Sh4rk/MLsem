open Types.Base
open Types
open Annot
open Env

val domain_of_proj : Parsing.Ast.projection -> typ -> typ
val proj : Parsing.Ast.projection -> typ -> typ
val typeof_proj : Parsing.Ast.projection -> TyScheme.t

exception Untypeable of Parsing.Ast.exprid * string

val typeof : Env.t -> Annot.t -> Ast.t -> typ
val generalize : e:Ast.t -> Env.t -> typ -> TyScheme.t
val typeof_def : Env.t -> Annot.t -> Ast.t -> TyScheme.t
