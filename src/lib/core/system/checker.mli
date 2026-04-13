open Mlsem_common
open Mlsem_types
open Annot

val is_type_test_unsat : tau:GTy.t -> GTy.t -> Ty.t
(** [is_type_test_unsat ~tau ty] returns a type that is empty
   if and only if a branch [tau] of a typecase on an expression of type [ty]
   is unreachable *)

type error = { eid: Eid.t ; title: string ; descr: string option }
exception Untypeable of error

val typeof : Env.t -> Annot.t -> Ast.t -> GTy.t
val generalize : e:Ast.t -> Env.t -> GTy.t -> TyScheme.t
val typeof_def : Env.t -> Annot.t -> Ast.t -> TyScheme.t
