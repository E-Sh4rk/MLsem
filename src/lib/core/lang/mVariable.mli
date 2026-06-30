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

(* May raise Invalid_argument *)
val add_to_env : Variable.t -> TyScheme.t -> Env.t -> Env.t
val replace_in_env : Variable.t -> TyScheme.t -> Env.t -> Env.t

val ref_uninit : Variable.t -> SA.e
val ref_cons : Variable.t -> SA.t -> SA.e
val ref_get : Variable.t -> SA.e
val ref_assign : Variable.t -> SA.t -> SA.e
