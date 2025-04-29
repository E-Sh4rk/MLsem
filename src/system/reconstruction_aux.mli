open Types.Additions
open Parsing.Variable
open Msc
open Annotations

(**
The following functor implements the auxiliary reconstruction system
(conversion of intermediate annotations into full annotations
for the algorithmic type system). It is presented as a functor
in order to have more control on the memoisation.
*)

module Make () : sig

    (** Functions relative to the caching/memoisation. *)

    (** [init e] should be called once before calling [infer_poly]
        or [infer_poly_a] on a (sub)-expression of [e]. *)
    val init : e -> unit

    (** [fv_def x] returns the free variables of the definition
        associated to [x]. In O(1). *)
    val fv_def : Variable.t -> VarSet.t
    
    (** [clear_cache ()] clears the cache used for memoisation
        and the free-variable hashtable.
        After calling this function, [init] should be called again
        before any call to [infer_poly] or [infer_poly_a]. *)
    val clear_cache : unit -> unit

    (** Auxiliary reconstruction algorithm for MSC forms. *)
    val infer_poly_a : Variable.t -> type_env -> Env.t -> PartialAnnot.a ->
        a -> FullAnnot.a_cached

    (** Auxiliary reconstruction algorithm for atoms. *)
    val infer_poly : type_env -> Env.t -> PartialAnnot.t -> e -> FullAnnot.t_cached
end