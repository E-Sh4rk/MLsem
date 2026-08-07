open Mlsem_common
open Mlsem_types
module SA = Mlsem_system.Ast

type t = Variable.t
type kind = Immut | AnnotMut of GTy.t | Mut

val create : kind -> string option -> t
val refresh : kind -> t -> t
val is_mutable : Variable.t -> bool
val kind : Variable.t -> kind
val kind_equal : kind -> kind -> bool
val kind_compat : kind -> kind -> bool

val add_to_env : Variable.t -> TyScheme.t -> Env.t -> Env.t
(** Binds a top-level variable, giving a mutable one the type its declaration
    fixes rather than the type inferred for its initializer.
    @raise Invalid_argument if the variable is mutable and its declared type
    contains type variables, or if the inferred type does not match it. *)

val replace_in_env : Variable.t -> TyScheme.t -> Env.t -> Env.t
(** @raise Invalid_argument as {!add_to_env}. *)

(** The four operations a mutable variable is encoded with, on an abstract
    reference type. Each raises [Invalid_argument] if the variable is not
    mutable. *)

val ref_uninit : Variable.t -> SA.e
(** Creation of an uninitialized cell.
    @raise Invalid_argument if the variable is not mutable. *)

val ref_cons : Variable.t -> SA.t -> SA.e
(** Creation of a cell holding the given expression.
    @raise Invalid_argument if the variable is not mutable. *)

val ref_get : Variable.t -> SA.e
(** Read of the cell.
    @raise Invalid_argument if the variable is not mutable. *)

val ref_assign : Variable.t -> SA.t -> SA.e
(** Write of the given expression into the cell.
    @raise Invalid_argument if the variable is not mutable. *)
