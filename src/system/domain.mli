open Types.Tvar
open Annot

type t
val empty : t
val add : IAnnot.coverage -> t -> t
val covers : TVarSet.t -> t -> IAnnot.coverage -> bool
val pp : Format.formatter -> t -> unit
