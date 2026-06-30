open Mlsem_types

let value_restriction = ref true

let infer_overload = ref true
let reexplore_failed_domains = ref true


let normalization_fun : (Ty.t -> Ty.t) ref = ref Heuristics.normalize_empty_abstracts

let subst_normalization_fun : (Heuristics.tally_context -> Subst.t list -> Subst.t list) ref =
  ref Heuristics.normalize_abstract_factors
