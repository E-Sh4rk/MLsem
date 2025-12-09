open Base
open Mlsem_utils

module Row = Sstt.Row

type kind = KNoInfer | KInfer | KTemporary

module type Var = sig
    type set
    type t

    val all_vars : kind -> set
    val has_kind : kind -> t -> bool
    val kind : t -> kind
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val name : t -> string
    val mk : kind -> string option -> t
    val pp : Format.formatter -> t -> unit
end
module type TVar = sig
    include Var
    val typ : t -> Ty.t
end
module type RVar = sig
    include Var
    val row : t -> Row.t
    val fty : t -> FTy.t
end

module type VarSet = sig
    type var

    include Set.S with type elt=var

    val union_many : t list -> t
    val inter_many : t list -> t
    val pp : Format.formatter -> t -> unit
end

module Var(V:Sstt.NamedIdentifier)(VS:Set.S with type elt=V.t)(P:sig val prefix:string end) = struct
  type t = V.t

  module VH = Hashtbl.Make(V)

  type vardata = {
    kind: kind
  }

  let data = VH.create 100
  let allvars = Hashtbl.create 100
  let has_kind k t =
    try (VH.find data t).kind = k with Not_found -> false
  let kind t = (VH.find data t).kind
  let all_vars k =
    match Hashtbl.find_opt allvars k with None -> VS.empty | Some vs -> vs
  let equal = V.equal
  let compare = V.compare
  let name = V.name

  let unique_id =
    let last = ref 0 in
    (fun () ->
      last := !last + 1 ; !last)

  let mk kind name =
    let id = unique_id () in
    let norm_name = (match kind with
      | KNoInfer -> P.prefix^"N"
      | KInfer -> P.prefix^"I"
      | KTemporary -> P.prefix^"T"
      )^(string_of_int id) in
    let name = match name with None -> norm_name | Some str -> P.prefix^str in
    let var = V.mk name in
    VH.add data var {kind} ;
    Hashtbl.replace allvars kind (all_vars kind |> VS.add var) ;
    var

  let pp = V.pp
end

module TVar = struct
  include Var(Sstt.Var)(Sstt.VarSet)(struct let prefix="'" end)
  let typ v = Sstt.VDescr.mk_var v |> Sstt.Ty.of_def
end

module RVar = struct
  include Var(Sstt.RowVar)(Sstt.RowVarSet)(struct let prefix="`" end)
  let row v = Row.id_for v
  let fty v = Sstt.Ty.F.mk_var v
end

module TVarSet = struct
  include Sstt.VarSet
  let union_many = List.fold_left union empty
  let inter_many = List.fold_left inter empty
  let pp fmt t =
    elements t |> Format.fprintf fmt "%a@." (Utils.pp_list TVar.pp)
end

module RVarSet = struct
  include Sstt.RowVarSet
  let union_many = List.fold_left union empty
  let inter_many = List.fold_left inter empty
  let pp fmt t =
    elements t |> Format.fprintf fmt "%a@." (Utils.pp_list RVar.pp)
end

module MVarSet = Sstt.MixVarSet

module Subst = Sstt.Subst

module TVOp = struct
  type field_ctx = Sstt.Tallying.field_ctx
  let get_field_ctx = Sstt.Tallying.get_field_ctx
  let decorrelate_fields = Sstt.Tallying.decorrelate_fields
  let recombine_fields = Sstt.Tallying.recombine_fields
  let recombine_fields' = Sstt.Tallying.recombine_fields'
  let fvars_associated_with = Sstt.Tallying.fvars_associated_with
  let rvar_associated_with ctx rv =
    Sstt.Tallying.rvar_associated_with ctx rv |>
      Option.map (fun (rv,lbl) -> rv, Record.from_label lbl)

  let vars = Sstt.Ty.all_vars
  let vars' ts = List.map vars ts |> List.fold_left MVarSet.union MVarSet.empty
  let top_vars = Sstt.Ty.all_vars_toplevel
  let strict_vars t =
    let res = ref MVarSet.empty in
    let _ = Sstt.Ty.def t |> Sstt.VDescr.map_nodes (fun ty ->
      res := MVarSet.union !res (Sstt.Ty.all_vars ty) ; ty
    ) in
    !res
  let vars_of_kind kind t =
    MVarSet.filter (TVar.has_kind kind) (RVar.has_kind kind) (vars t)
  let vpol = Sstt.Var.mk "__pol__" |> Sstt.Ty.mk_var
  let rpol = Sstt.RowVar.mk "__pol__" |> Sstt.Ty.F.mk_var
  let polarity1 v t =
    let vt = Sstt.Ty.mk_var v in
    let to_smaller = Sstt.Subst.singleton1 v (Sstt.Ty.cap vt vpol) in
    let to_larger = Sstt.Subst.singleton1 v (Sstt.Ty.cup vt vpol) in
    let cov = Sstt.Ty.leq (Subst.apply to_smaller t) t in
    let contrav = Sstt.Ty.leq (Subst.apply to_larger t) t in
    if cov && contrav then `None
    else if cov then `Pos
    else if contrav then `Neg
    else `Both
  let polarity2 v t =
    let vt = Sstt.Ty.F.mk_var v in
    let to_smaller = Sstt.Subst.singleton2 v
      (Sstt.Ty.F.cap vt rpol |> Sstt.Row.all_fields) in
    let to_larger = Sstt.Subst.singleton2 v
      (Sstt.Ty.F.cup vt rpol |> Sstt.Row.all_fields) in
    let cov = Sstt.Ty.leq (Subst.apply to_smaller t) t in
    let contrav = Sstt.Ty.leq (Subst.apply to_larger t) t in
    if cov && contrav then `None
    else if cov then `Pos
    else if contrav then `Neg
    else `Both
  let polarity' f v ts =
    let aux acc n =
      match acc, n with
      | `Both, _ | _, `Both -> `Both
      | `None, p | p, `None -> p
      | `Neg, `Neg -> `Neg
      | `Pos, `Pos -> `Pos
      | `Neg, `Pos | `Pos, `Neg -> `Both
    in
    List.fold_left aux `None (List.map (f v) ts)
  let polarity1' = polarity' polarity1
  let polarity2' = polarity' polarity2
  let vars_with_polarity1' ts =
    vars' ts |> Sstt.MixVarSet.elements1 |> List.filter_map (fun v ->
      match polarity1' v ts with
      | `None -> None
      | `Pos -> Some (v, `Pos) | `Neg -> Some (v, `Neg) | `Both -> Some (v, `Both)
    )
  let vars_with_polarity2' ts =
    vars' ts |> Sstt.MixVarSet.elements2 |> List.filter_map (fun v ->
      match polarity2' v ts with
      | `None -> None
      | `Pos -> Some (v, `Pos) | `Neg -> Some (v, `Neg) | `Both -> Some (v, `Both)
    )
  let vars_with_polarity1 t = vars_with_polarity1' [t]
  let vars_with_polarity2 t = vars_with_polarity2' [t]
  let is_ground_typ t = vars t |> MVarSet.is_empty

  let refresh ~kind vars =
    let f1 v = (v, TVar.mk kind None |> TVar.typ) in
    let f2 v = (v, RVar.mk kind None |> RVar.row) in
    let l1 = vars |> MVarSet.elements1 |> List.map f1 in
    let l2 = vars |> MVarSet.elements2 |> List.map f2 in
    Subst.of_list l1 l2

  let shorten_names vs =
    let char i = Char.chr ((i mod 26)+97) in
    let nb i = i / 26 in
    let names1 =
      let c = ref 0 in
      fun _ ->
        let letter, n = char !c, nb !c in
        c := !c + 1 ;
        if n = 0 then
          Format.asprintf "'%c" letter
        else
          Format.asprintf "'%c%i" letter n
    in
    let names2 =
      let c = ref 0 in
      fun _ ->
        let letter, n = char !c, nb !c in
        c := !c + 1 ;
        if n = 0 then
          Format.asprintf "`%c" letter
        else
          Format.asprintf "`%c%i" letter n
    in
    let (s,_) = Sstt.Subst.refresh ~names1 ~names2 vs in
    s

  let pp_typ_short fmt t =
    Ty.pp' (vars t |> shorten_names) fmt t
  let pp_typ_subst s fmt t =
    Ty.pp' s fmt t

  let clean_subst' ~pos1 ~neg1 ~pos2 ~neg2 mono ts =
    let b1 = vars_with_polarity1' ts |>
    List.filter_map (fun (v,p) ->
        if MVarSet.mem1 v mono then None
        else match p with
        | `Pos -> Some (v, pos1)
        | `Neg -> Some (v, neg1)
        | `Both -> None
    ) in
    let b2 = vars_with_polarity2' ts |>
    List.filter_map (fun (v,p) ->
        if MVarSet.mem2 v mono then None
        else match p with
        | `Pos -> Some (v, pos2)
        | `Neg -> Some (v, neg2)
        | `Both -> None
    ) in
  Subst.of_list b1 b2
  let clean_subst ~pos1 ~neg1 ~pos2 ~neg2 mono t =
    clean_subst' ~pos1 ~neg1 ~pos2 ~neg2 mono [t]

  let clean' ~pos1 ~neg1 ~pos2 ~neg2 mono ts =
    let s = clean_subst' ~pos1 ~neg1 ~pos2 ~neg2 mono ts in
    List.map (Subst.apply s) ts
  let clean ~pos1 ~neg1 ~pos2 ~neg2 mono t =
    clean' ~pos1 ~neg1 ~pos2 ~neg2 mono [t] |> List.hd

  let bot_instance mono ty =
    let fc = get_field_ctx (MVarSet.proj2 mono) [ty] in
    decorrelate_fields fc ty
    |> clean ~pos1:Ty.empty ~neg1:Ty.any ~pos2:Row.empty ~neg2:Row.any mono
    |> recombine_fields fc

  let top_instance mono ty =
    let fc = get_field_ctx (MVarSet.proj2 mono) [ty] in
    decorrelate_fields fc ty
    |> clean ~pos1:Ty.any ~neg1:Ty.empty ~pos2:Row.any ~neg2:Row.empty mono
    |> recombine_fields fc

  let tallying mono cs =
    Recording_internal.record mono cs ;
    Sstt.Tallying.tally mono cs
  let tallying_fields mono cs =
    Recording_internal.record mono cs ;
    Sstt.Tallying.tally_fields mono cs
  let decompose mono t1 t2 =
    Sstt.Tallying.decompose mono t1 t2

  let factorize (pvs, nvs) t =
    let dnf = Sstt.Ty.def t |> Sstt.VDescr.dnf in
    let factor (pvs',nvs',descr) =
      let pvs', nvs' = TVarSet.of_list pvs', TVarSet.of_list nvs' in
      if TVarSet.subset pvs pvs' then
        let pvs', nvs' = TVarSet.diff pvs' pvs, TVarSet.diff nvs' nvs in
        Some (TVarSet.elements pvs', TVarSet.elements nvs', descr)
      else
        None
    in
    let fact = dnf |> List.filter_map factor in
    let nfact = dnf |> List.filter (fun line -> factor line = None) in
    Sstt.VDescr.of_dnf fact |> Sstt.Ty.of_def, Sstt.VDescr.of_dnf nfact |> Sstt.Ty.of_def
end
