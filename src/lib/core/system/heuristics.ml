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

module Position = Sstt.Dependencies.Position
module PathSet = Set.Make(struct
  type t = Position.t list
  let compare = List.compare Position.compare
end)

(* For each variable of [ty], the paths (sequences of positions, in the sense of
   [Sstt.Dependencies]) leading from the root of [ty] to its occurrences. The path of
   a row variable ends with the field it occurs in. A recursive type is explored until
   a node repeats on the current path.
   Also returns the variables that occur at the top level of a node (or of a field)
   together with another variable of the same sort. *)
let positions ty =
  let open Sstt in
  let tvs = ref VarMap.empty and rvs = ref RowVarMap.empty in
  let shared = ref MVarSet.empty in
  let add p = function None -> Some (PathSet.singleton p) | Some ps -> Some (PathSet.add p ps) in
  let rec aux visited path n =
    if not (MVarSet.is_empty (Ty.all_vars n) || Dependencies.NodeSet.mem n visited) then begin
      let visited = Dependencies.NodeSet.add n visited in
      let p = List.rev path in
      let vs = Ty.vars_toplevel n in
      if VarSet.cardinal vs > 1 then shared := MVarSet.union !shared (MVarSet.of_set1 vs) ;
      vs |> VarSet.iter (fun v -> tvs := VarMap.update v (add p) !tvs) ;
      let add_row_vars pos f =
        let vs = Row.all_fields f |> Row.row_vars_toplevel in
        if RowVarSet.cardinal vs > 1 then shared := MVarSet.union !shared (MVarSet.of_set2 vs) ;
        vs |> RowVarSet.iter (fun v -> rvs := RowVarMap.update v (add (p@[pos])) !rvs) in
      let _ = Ty.def n |> VDescr.map (fun d ->
        let _ = Descr.get_records d |> Records.map (fun a ->
          Records.Atom.dom a |> LabelSet.elements |> List.iter (fun lbl ->
            add_row_vars (Position.Field lbl) (Records.Atom.find lbl a)) ;
          add_row_vars Position.Tail a.Records.Atom.tail ; a) in
        d) in
      Dependencies.of_ty n |> Dependencies.PosMap.iter (fun pos ns ->
        Dependencies.NodeSet.iter (aux visited (pos::path)) ns)
    end
  in
  aux Dependencies.NodeSet.empty [] ty ;
  !tvs, !rvs, !shared

(* The signature of a variable gives, for each type, the paths to its occurrences
   and its variance. Two variables with the same signature play the same role. *)
module Signature = Map.Make(struct
  type t = (PathSet.t * [ `Both | `Neg | `Pos | `None ]) list
  let compare =
    let rank = function `None -> 0 | `Pos -> 1 | `Neg -> 2 | `Both -> 3 in
    List.compare (fun (ps1,v1) (ps2,v2) ->
      let c = PathSet.compare ps1 ps2 in
      if c <> 0 then c else Int.compare (rank v1) (rank v2))
end)

(* Variables with the same signature are mapped to the first one of their group. *)
let merge_same_signature ~elements ~mk_subst sigs =
  let groups = elements |> List.fold_left (fun acc v ->
    Signature.update (sigs v) (function None -> Some [v] | Some vs -> Some (v::vs)) acc
  ) Signature.empty in
  Signature.fold (fun _ vs acc ->
    match List.rev vs with
    | [] -> acc
    | rep::others -> List.map (fun v -> v, rep) others @ acc
  ) groups [] |> mk_subst

let assimilate_vars vs ty =
  let tys = [GTy.lb ty ; GTy.ub ty] in
  let occs = tys |> List.map (fun ty -> ty, positions ty) in
  let shared = occs |> List.fold_left (fun acc (_,(_,_,sh)) -> MVarSet.union acc sh) MVarSet.empty in
  let vs = MVarSet.diff vs shared in
  let sigs find polarity v = occs |> List.map (fun (ty, occ) ->
    match find v occ with
    | None -> PathSet.empty, `None
    | Some ps -> ps, polarity v ty
  ) in
  let s1 = merge_same_signature ~elements:(MVarSet.elements1 vs)
    ~mk_subst:(fun l -> l |> List.map (fun (v,v') -> v, TVar.typ v') |> Subst.of_list1)
    (sigs (fun v (tvs,_,_) -> Sstt.VarMap.find_opt v tvs) TVOp.polarity1) in
  let s2 = merge_same_signature ~elements:(MVarSet.elements2 vs)
    ~mk_subst:(fun l -> l |> List.map (fun (v,v') -> v, RVar.row v') |> Subst.of_list2)
    (sigs (fun v (_,rvs,_) -> Sstt.RowVarMap.find_opt v rvs) TVOp.polarity2) in
  Subst.combine s1 s2
