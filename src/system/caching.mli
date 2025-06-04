open Annot
open Env
open Types.Tvar

module Domain : sig
    type t
    val empty : t
    val add : IAnnot.coverage -> t -> t
    val covers : t -> IAnnot.coverage -> bool
    val pp : Format.formatter -> t -> unit
end

module Cache : sig
    type 'a t
    val empty : unit -> 'a t
    val add : Ast.t -> Env.t -> IAnnot.t -> 'a -> 'a t -> unit
    val get : Ast.t -> Env.t -> IAnnot.t -> 'a t -> 'a option
end

module TVCache : sig
    type t
    val empty : unit -> t
    val get : t -> Parsing.Ast.exprid -> TVar.t -> TVar.t
    val get' : t -> Parsing.Ast.exprid -> TVarSet.t -> Subst.t
    val res_tvar : TVar.t
    val res_tvar' : int -> TVar.t
end