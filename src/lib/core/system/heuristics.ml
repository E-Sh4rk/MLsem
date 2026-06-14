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


type tally_context = { mono: MVarSet.t ; tvars: MVarSet.t ; res: Ty.t }
let abstract_factors v ty =
  let (factor, _) = TVOp.factorize (TVarSet.singleton v, TVarSet.empty) ty in
  let res = ref [] in
  let aux (abs, dnf) =
    dnf |> List.filter (fun (ps, ns) ->
      if ps = [] then true
      else
        let ps = ps |> List.map (Abstract.mk abs) |> Ty.conj in
        let ns = ns |> List.map (Abstract.mk abs) |> List.map Ty.neg |> Ty.conj in
        res := (Ty.cap ps ns)::(!res) ; false
    )
  in
  let remaining = Abstract.top_transform aux factor in
  match !res with
  | [] -> [ Subst.identity ]
  | res -> (remaining::res) |> List.map (fun ty -> Subst.singleton1 v ty)
let abstract_factors sols (v,t) =
  let ss = abstract_factors v t in
  sols |> List.concat_map (fun sol -> List.map (fun s -> Subst.compose s sol) ss)
let abstract_factors tvars sol =
  (* Note: this simplification does nothing if parameters are fully annotated *)
  List.fold_left abstract_factors [sol]
    (Subst.restrict tvars sol |> Subst.bindings1)

let normalize_abstract_factors ctx sols =
  sols |> List.concat_map (abstract_factors ctx.tvars)
