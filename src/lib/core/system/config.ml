open Mlsem_types

let normalize_empty_abstracts =
  let aux (_, dnf) =
    let has_no_empty lst =
      List.for_all Ty.non_empty lst
    in
    let has_no_pos_empty (ps,_) =
      List.for_all has_no_empty ps
    in
    let remove_neg_empty (ps,ns) =
      ps, List.filter has_no_empty ns
    in
    List.filter has_no_pos_empty dnf |> List.map remove_neg_empty
  in
  Abstract.transform aux

(* ---------------- *)

let value_restriction = ref true

let infer_overload = ref true

let normalization_fun : (Ty.t -> Ty.t) ref = ref normalize_empty_abstracts

let no_abstract_inter = ref true
