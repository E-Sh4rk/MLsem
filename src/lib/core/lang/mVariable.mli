open Mlsem_common
open Mlsem_types

type t = Variable.t
type kind = Immut | AnnotMut of Ty.t | Mut

val create : kind -> string option -> t
val refresh : kind -> t -> t
val is_mutable : Variable.t -> bool
val kind : Variable.t -> kind
val kind_equal : kind -> kind -> bool
val kind_leq : kind -> kind -> bool

(* May raise Invalid_argument *)
val add_to_env : Variable.t -> TyScheme.t -> Env.t -> Env.t

val ref_uninit : Variable.t -> TyScheme.t
val ref_cons : Variable.t -> TyScheme.t
val ref_get : Variable.t -> TyScheme.t
val ref_assign : Variable.t -> TyScheme.t
