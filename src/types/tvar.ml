
module type TVar = sig
  type set
  type t = Sstt.Var.t

  val user_vars : unit -> set
  val from_user : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val name : t -> string
  val display_name : t -> string

  val mk : ?user:bool -> string option -> t
  val typ : t -> Base.typ

  val pp : Format.formatter -> t -> unit
end

module type TVarSet = sig
  type var
  type t

  val empty : t
  val construct : var list -> t
  val is_empty : t -> bool
  val mem : t -> var -> bool
  val filter : (var -> bool) -> t -> t
  val union : t -> t -> t
  val union_many : t list -> t
  val add : var -> t -> t
  val inter : t -> t -> t
  val inter_many : t list -> t
  val diff : t -> t -> t
  val rm : var -> t -> t
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val destruct : t -> var list
  val pp : Format.formatter -> t -> unit
end

module TVH = Hashtbl.Make(Sstt.Var)

module TVar = struct
  type t = Sstt.Var.t

  type vardata = {
    user: bool ;
    dname: string
  }

  let data = TVH.create 100
  let uservars = ref Sstt.VarSet.empty
  let from_user t =
    try (TVH.find data t).user with Not_found -> false
  let user_vars () = !uservars
  let equal = Sstt.Var.equal
  let compare = Sstt.Var.compare
  let name = Sstt.Var.name
  let display_name t =
    try (TVH.find data t).dname with Not_found -> name t

  let unique_id =
    let last = ref 0 in
    (fun () ->
      last := !last + 1 ; !last)

  let mk ?(user=false) name =
    let id = unique_id () in
    let norm_name = (if user then "" else "_")^(string_of_int id) in
    let name = match name with None -> norm_name | Some str -> str in
    let var = Sstt.Var.mk norm_name in
    TVH.add data var {user; dname=name} ;
    if user then uservars := Sstt.VarSet.add var (!uservars) ;
    var
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
let vars' ts = List.map vars ts |> TVarSet.union_many
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

let vars_user t =
  TVarSet.filter TVar.from_user (vars t)
let vars_internal t =
  TVarSet.filter (fun v -> TVar.from_user v |> not) (vars t)
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
let polarity' v ts =
  let aux acc n =
    match acc, n with
    | `Both, _ | _, `Both -> `Both
    | `None, p | p, `None -> p
    | `Neg, `Neg -> `Neg
    | `Pos, `Pos -> `Pos
    | `Neg, `Pos | `Pos, `Neg -> `Both
  in
  List.fold_left aux `None (List.map (polarity v) ts)
let vars_with_polarity' ts =
  vars' ts |> Sstt.VarSet.elements |> List.filter_map (fun v ->
    match polarity' v ts with
    | `None -> None
    | `Pos -> Some (v, `Pos) | `Neg -> Some (v, `Neg) | `Both -> Some (v, `Both)
  )
let vars_with_polarity t = vars_with_polarity' [t]
let is_ground_typ t = vars t |> TVarSet.is_empty

let refresh vars =
  let f v = (v, TVar.mk None |> TVar.typ) in
  vars |> TVarSet.destruct |> List.map f |> Subst.construct

let shorten_names vs =
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

let pp_typ_short fmt t =
  let t = Subst.apply (vars t |> shorten_names) t in
  Base.pp_typ fmt t

(* Operations on types *)

let clean_subst' ~pos ~neg mono ts =
  vars_with_polarity' ts |>
  List.filter_map (fun (v,p) ->
      if TVarSet.mem mono v then None
      else match p with
      | `Pos -> Some (v, pos)
      | `Neg -> Some (v, neg)
      | `Both -> None
  ) |> Subst.construct
let clean_subst ~pos ~neg mono t = clean_subst' ~pos ~neg mono [t]

let clean' ~pos ~neg mono ts =
  let s = clean_subst' ~pos ~neg mono ts in
  List.map (Subst.apply s) ts
let clean ~pos ~neg mono t = clean' ~pos ~neg mono [t] |> List.hd

let tallying mono cs =
  Sstt.Tallying.tally mono cs
let tallying_with_prio mono prio cs =
  Sstt.Tallying.tally_with_priority prio mono cs
let test_tallying mono cs = tallying mono cs <> []

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
