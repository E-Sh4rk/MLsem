open Mlsem_types

val normalize_empty_abstracts : Ty.t -> Ty.t

type tally_context = { mono: MVarSet.t ; tvars: MVarSet.t ; res: Ty.t }
val normalize_abstract_factors : tally_context -> Subst.t list -> Subst.t list
