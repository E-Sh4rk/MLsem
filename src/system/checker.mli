open Types.Base
open Types
open Annot
open Env

val typeof_const : Parsing.Ast.const -> typ
val domain_of_proj : Parsing.Ast.projection -> typ -> typ
val proj : Parsing.Ast.projection -> typ -> typ
val typeof_proj : Parsing.Ast.projection -> TyScheme.t

exception Untypeable of Parsing.Ast.exprid * string

val typeof : Env.t -> Annot.t -> Ast.t -> typ
