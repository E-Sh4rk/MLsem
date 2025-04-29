
module TVH = Hashtbl.Make(Sstt.Var)

module TVar = struct
  type t = Sstt.Var.t

  type vardata = {
    poly: bool;
    infer: bool ;
    dname: string
  }

  let data = TVH.create 100

  let is_unregistered t =
    TVH.mem data t |> not

  let is_poly t = (TVH.find data t).poly
  let is_mono t = is_poly t |> not
  let can_infer t = (TVH.find data t).infer

  let equal = Sstt.Var.equal
  let compare = Sstt.Var.compare
  let name = Sstt.Var.name
  let display_name t = (TVH.find data t).dname

  let unique_mono_id =
    let last = ref 0 in
    (fun () ->
      last := !last + 1 ; !last)

  let unique_poly_id =
    let last = ref 0 in
    (fun () ->
      last := !last + 1 ; !last)

  let unique_unregistered_id =
    let last = ref 0 in
    (fun () ->
      last := !last + 1 ; !last)

  let mk_mono ?(infer=true) name =
    let id = unique_mono_id () in
    let norm_name = (string_of_int id)^"M" in
    let name = match name with None -> norm_name | Some str -> str in
    let var = Sstt.Var.mk norm_name in
    TVH.add data var {poly=false; infer; dname=name} ;
    var
  let mk_poly name =
    let id = unique_poly_id () in
    let norm_name = (string_of_int id)^"P" in
    let name = match name with None -> norm_name | Some str -> str in
    let var = Sstt.Var.mk norm_name in
    TVH.add data var {poly=true; infer=true; dname=name} ;
    var
  let mk_fresh t =
    let dname = display_name t in
    if is_mono t then mk_mono (Some dname) else mk_poly (Some dname)
  let mk_unregistered () =
    let id = unique_unregistered_id () in
    let norm_name = (string_of_int id)^"U" in
    Sstt.Var.mk norm_name

  let typ = Sstt.Ty.mk_var

  let pp = Sstt.Var.pp
end

module TVarSet = struct
  type t = Sstt.VarSet.t
  let empty = Sstt.VarSet.empty
  let construct = Sstt.VarSet.of_list
  let is_empty = Sstt.VarSet.is_empty
  let mem t v = Sstt.VarSet.mem v t
  let filter = Sstt.VarSet.filter
  let union = Sstt.VarSet.union
  let union_many = List.fold_left union empty
  let add = Sstt.VarSet.add
  let inter = Sstt.VarSet.inter
  let inter_many = List.fold_left inter empty
  let diff = Sstt.VarSet.diff
  let rm = Sstt.VarSet.remove
  let equal = Sstt.VarSet.equal
  let subset = Sstt.VarSet.subset
  let destruct = Sstt.VarSet.elements
  let pp fmt t =
    destruct t |> Format.fprintf fmt "%a@." (Utils.pp_list TVar.pp)
end

let vars = Sstt.Ty.vars
let top_vars = Sstt.Ty.vars_toplevel
let check_var t =
  match top_vars t |> Sstt.VarSet.elements with
  | [v] when Sstt.Ty.equiv t (Sstt.Ty.mk_var v) -> `Pos v
  | [v] when Sstt.Ty.equiv t (Sstt.Ty.mk_var v |> Sstt.Ty.neg) -> `Neg v
  | _ -> `Not_var

module Subst = struct
  type t = Sstt.Subst.t
  let construct lst = lst |> Sstt.Subst.of_list
  let identity = Sstt.Subst.identity
  let destruct = Sstt.Subst.bindings
  let is_identity = Sstt.Subst.is_identity
  let apply = Sstt.Subst.apply
  let dom = Sstt.Subst.domain
  let mem s v = Sstt.VarSet.mem v (dom s)
  let rm = Sstt.Subst.remove
  let find = Sstt.Subst.find
  let equiv = Sstt.Subst.equiv

  let compose_restr s2 s1 = s1 |> Sstt.Subst.map (Sstt.Subst.apply s2)
  let compose = Sstt.Subst.compose
  let combine s1 s2 =
      assert (TVarSet.inter (dom s1) (dom s2) |> TVarSet.is_empty) ;
      let s1 = destruct s1 in
      let s2 = destruct s2 in
      s1@s2 |> construct
  let restrict s vars =
    Sstt.Subst.filter (fun v _ -> TVarSet.mem vars v) s
  let remove s vars =
    Sstt.Subst.filter (fun v _ -> TVarSet.mem vars v |> not) s
  let split s vars =
      (restrict s vars, remove s vars)
  let vars s =
      destruct s |> List.map (fun (v, t) -> TVarSet.rm v (vars t))
      |> TVarSet.union_many
  let is_renaming t =
    destruct t |>
    List.for_all (fun (_,t) ->
      match check_var t with
      | `Pos _ -> true
      | _ -> false)

(* let pp_entry fmt (v,t) =
    Format.fprintf fmt "%a ===> %a" pp_var v pp_typ t
  let pp fmt t =
    Format.fprintf fmt "%a@." (Utils.pp_long_list pp_entry) (destruct t) *)
  let pp = Base.pp_subst
end

let vars_mono t =
  TVarSet.filter TVar.is_mono (vars t)
let vars_poly t =
  TVarSet.filter TVar.is_poly (vars t)
let vars_infer t =
  TVarSet.filter TVar.can_infer (vars t)
let vpol = Sstt.Var.mk "__pol__" |> Sstt.Ty.mk_var
let polarity v t =
  let vt = Sstt.Ty.mk_var v in
  let to_smaller = Sstt.Subst.singleton v (Sstt.Ty.cap vt vpol) in
  let to_larger = Sstt.Subst.singleton v (Sstt.Ty.cup vt vpol) in
  let cov = Sstt.Ty.leq (Subst.apply to_smaller t) t in
  let contrav = Sstt.Ty.leq (Subst.apply to_larger t) t in
  if cov && contrav then `None
  else if cov then `Pos
  else if contrav then `Neg
  else `Both
let vars_with_polarity t =
  Sstt.Ty.vars t |> Sstt.VarSet.elements |> List.filter_map (fun v ->
    match polarity v t with
    | `None -> None
    | `Pos -> Some (v, `Pos) | `Neg -> Some (v, `Neg) | `Both -> Some (v, `Both)
  )
let is_mono_typ t = vars_poly t |> TVarSet.is_empty
let is_novar_typ t = vars t |> TVarSet.is_empty

let refresh vars =
  let f v = (v, TVar.mk_fresh v |> TVar.typ) in
  vars |> TVarSet.destruct |> List.map f |> Subst.construct

let generalize' vars =
  let f v =
    let pv = TVar.mk_poly None in
    (v, TVar.typ pv), (pv, TVar.typ v)
  in
  let (s, s') = vars |>
    TVarSet.filter TVar.is_mono |>
    TVarSet.destruct |> List.map f |> List.split in
  Subst.construct s, Subst.construct s'
let generalize vars =
  let f v = (v, TVar.mk_poly None |> TVar.typ) in
  vars |>
    TVarSet.filter TVar.is_mono |>
    TVarSet.destruct |> List.map f |> Subst.construct
let monomorphize vars =
  let f v = (v, TVar.mk_mono None |> TVar.typ) in
  vars |>
    TVarSet.filter TVar.is_poly |>
    TVarSet.destruct |> List.map f |> Subst.construct

(* let generalize_unregistered vars =
  let f v = (v, TVar.mk_poly None |> TVar.typ) in
  vars |>
    TVarSet.filter TVar.is_unregistered |>
    TVarSet.destruct |> List.map f |> Subst.construct *)
(* let monomorphize_unregistered vars =
  let f v = (v, TVar.mk_mono None |> TVar.typ) in
  vars |>
    TVarSet.filter TVar.is_unregistered |>
    TVarSet.destruct |> List.map f |> Subst.construct *)

let pp_typ_short fmt t =
  let short_names vs =
    let char i = Char.chr ((i mod 26)+97) in
    let nb i = i / 26 in
    let names =
      let c = ref 0 in
      fun _ ->
        let letter, n = char !c, nb !c in
        c := !c + 1 ;
        if n = 0 then
          Format.asprintf "'%c" letter
        else
          Format.asprintf "'%c%i" letter n
    in
    let (s,_) = Sstt.Subst.refresh ~names vs in
    s
  in
  let t = Subst.apply (short_names (vars t)) t in
  Base.pp_typ fmt t
let string_of_type_short t =
  Format.asprintf "%a" pp_typ_short t

(* Operations on types *)

module Raw = struct
  (* Tallying *)
  let clean_type ~pos ~neg mono t =
    let vars = TVarSet.diff (Sstt.Ty.vars t) mono in
    let s = vars |> TVarSet.destruct |> List.filter_map (fun v ->
      match polarity v t with
      | `None | `Both -> None
      | `Pos -> Some (v, pos)
      | `Neg -> Some (v, neg)
    ) |> Subst.construct
    in
    Subst.apply s t

  let [@warning "-32"] print_tallying_instance delta constr =
    Format.printf "Constraints:@." ;
    let allvars = ref TVarSet.empty in
    constr |> List.iter (fun (l,r) ->
        allvars := TVarSet.union (!allvars) (vars l) ;
        allvars := TVarSet.union (!allvars) (vars r) ;
        Format.printf "(%a, %a)@." Base.pp_typ l Base.pp_typ r ;
    );
    Format.printf "With delta=%a, and var order=%a@."
        (Utils.pp_list TVar.pp) (TVarSet.destruct delta)
        (Utils.pp_list TVar.pp) (TVarSet.destruct !allvars)

  let [@warning "-32"] check_tallying_solution constr res =
    let error = ref false in
    let res =
        res |> List.filter_map (fun s ->
        if (constr |> List.for_all (fun (l,r) ->
                Base.subtype (Subst.apply s l) (Subst.apply s r)
            ))
        then Some s else begin
            error := true ;
            (* Format.printf "INVALID SOLUTION REMOVED: %a@." Subst.pp s ; *)
            None
        end
    )
    in
    if !error then begin
        Format.printf "===== WARNING: Cduce tallying issue.@. ====="
    end ; res

  let tallying d cs =
      Sstt.Tallying.tally d cs
      (* |> (check_tallying_solution cs) *)
  let tallying_with_prio prio d cs =
    Sstt.Tallying.tally_with_priority prio d cs

  let test_tallying d cs = tallying d cs <> []
end

let clean_type ~pos ~neg t =
  Raw.clean_type ~pos ~neg (vars_mono t) t

let clean_type_subst ~pos ~neg t =
  vars_with_polarity t |>
  List.filter_map (fun (v,p) ->
      if TVar.is_mono v then None
      else match p with
      | `Pos -> Some (v, pos)
      | `Neg -> Some (v, neg)
      | `Both -> None
  ) |> Subst.construct

let test_tallying constr =
  let mono = constr |>
    List.map (fun (a,b) -> [vars_mono a ; vars_mono b]) |>
    List.flatten in
  let mono = TVarSet.union_many mono in
  Raw.test_tallying mono constr

let tallying ?prio constr =
  let vars = constr |>
    List.map (fun (a,b) -> [vars a ; vars b]) |>
    List.flatten |> TVarSet.union_many in
  let mono = vars |> TVarSet.filter TVar.is_mono in
  match prio with
  | None -> Raw.tallying mono constr
  | Some prio -> Raw.tallying_with_prio prio mono constr

let tallying_infer ?(noprio=[]) constr =
  let infer = constr |>
    List.map (fun (a,b) -> [vars_infer a ; vars_infer b]) |>
    List.flatten |> TVarSet.union_many in
  let prio = infer |> TVarSet.filter TVar.is_mono in
  let prio = TVarSet.diff prio (TVarSet.construct noprio) |> TVarSet.destruct in
  let (gen, mono) = generalize' infer in
  let constr = constr |>
    List.map (fun (a,b) ->
      let r1 = refresh (vars_poly a) in
      let r2 = refresh (vars_poly b) in
      let a = Subst.apply r1 a in
      let b = Subst.apply r2 b in
      (Subst.apply gen a, Subst.apply gen b))
  in
  let res = tallying ~prio constr |> List.map (fun s ->
    let s = Subst.compose_restr s gen in
    Subst.compose_restr mono s
  ) in
  (* Monomorphize remaining vars *)
  let vars = res |> List.map (fun s -> Subst.vars s) |> TVarSet.union_many in
  let mono = monomorphize vars in
  res |> List.map (fun s -> Subst.compose_restr mono s)

let factorize (pvs, nvs) t =
  let dnf = Sstt.Ty.def t |> Sstt.VDescr.dnf in
  let factor (pvs',nvs',descr) =
    let pvs', nvs' = TVarSet.construct pvs', TVarSet.construct nvs' in
    if TVarSet.subset pvs pvs' then
      let pvs', nvs' = TVarSet.diff pvs' pvs, TVarSet.diff nvs' nvs in
      Some (TVarSet.destruct pvs', TVarSet.destruct nvs', descr)
    else
      None
  in
  let fact = dnf |> List.filter_map factor in
  let nfact = dnf |> List.filter (fun line -> factor line = None) in
  Sstt.VDescr.of_dnf fact |> Sstt.Ty.of_def, Sstt.VDescr.of_dnf nfact |> Sstt.Ty.of_def
