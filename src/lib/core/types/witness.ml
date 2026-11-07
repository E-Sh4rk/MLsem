open Sstt

type t =
  | Other
  | Int of Z.t
  | Enum of Enum.t
  | EnumOther
  | Tuple of t list
  | Record of (Label.t * tf) list * tf
  | Tag of Tag.t * t
  | TagOther of Tag.t
  | Arrow
and tf =
  | Absent
  | Present of t

exception Found of t
let found t = raise (Found t)

let rec all_some = function
  | [] -> Some []
  | None::_ -> None
  | (Some x)::l -> Option.map (fun l -> x::l) (all_some l)

(* Depth-limited search: [fuel] bounds the depth of the witness, i.e. the
   number of nested constructors (tuples, records, tags) having a non-trivial
   content. *)
let rec of_ty' fuel ty =
  if fuel < 0 then None
  else try Ty.def ty |> of_vdescr fuel ; None with Found t -> Some t
and of_vdescr fuel vd =
  let lines = VDescr.dnf vd |> List.map (fun (_,_,d) -> d) in
  (* Less informative witnesses are only considered if no other witness
     is found, in any line of the DNF *)
  lines |> List.iter (of_descr fuel) ;
  lines |> List.iter of_descr_fallback
and of_descr fuel d =
  Descr.get_intervals d |> of_intervals ;
  Descr.get_enums d |> of_finite_enums ;
  let fuel = fuel - 1 in
  Descr.get_tuples d |> of_tuples fuel ;
  Descr.get_records d |> of_records fuel ;
  Descr.get_tags d |> of_tags fuel
and of_descr_fallback d =
  Descr.get_enums d |> of_infinite_enums ;
  Descr.get_arrows d |> of_arrows ;
  Descr.get_others d |> of_others
(* [Enums.destruct] returns [(true, enums)] for exactly [enums],
   and [(false, enums)] for all constants but [enums]. *)
and of_finite_enums e =
  match Enums.destruct e with
  | true, e::_ -> found (Enum e)
  | _, _ -> ()
and of_infinite_enums e =
  match Enums.destruct e with
  | false, _ -> found EnumOther
  | true, _ -> ()
and of_intervals i =
  match Intervals.destruct i with
  | [] -> ()
  | i::_ ->
    begin match Intervals.Atom.lb i, Intervals.Atom.ub i with
    | None, None -> found (Int Z.zero)
    | Some z, _ | _, Some z -> found (Int z)
    end
and of_arrows a =
  if Descr.mk_arrows a |> Ty.mk_descr |> Ty.is_empty |> not then found Arrow
and of_others o =
  if o then found Other
and of_tuples fuel t =
  let comps, others = Tuples.components t in
  comps |> List.iter (fun c ->
    Op.TupleComp.as_union c |> List.iter (fun atom ->
      match List.map (of_ty' fuel) atom |> all_some with
      | Some ws -> found (Tuple ws)
      | None -> ())) ;
  if others then
    (* Tuples of any arity not listed in [comps] are included *)
    let n = comps |> List.fold_left (fun acc c -> max acc (TupleComp.len c + 1)) 0 in
    found (Tuple (List.init n (fun _ -> Other)))
and of_field fuel oty =
  let ty, opt = Ty.O.get oty in
  if opt then Some Absent
  else of_ty' fuel ty |> Option.map (fun w -> Present w)
and of_records fuel r =
  Op.Records.as_union r |> List.iter (fun (a:Op.Records.Atom.t) ->
    let bindings =
      Op.Records.Atom.LabelMap.bindings a.bindings
      |> List.map (fun (l,f) -> of_field fuel f |> Option.map (fun w -> (l,w)))
      |> all_some
    in
    match bindings, of_field fuel a.tail with
    | Some bs, Some tl -> found (Record (bs, tl))
    | _, _ -> ())
and of_tags fuel t =
  let comps, _ = Tags.components t in
  comps |> List.iter (fun c ->
    let tag = TagComp.tag c in
    let of_param ty =
      match of_ty' fuel ty with Some w -> found (Tag (tag, w)) | None -> ()
    in
    if Op.TagComp.is_identity c then
      Op.TagComp.as_atom c |> snd |> of_param
    else if Op.TagComp.preserves_cap c then
      Op.TagComp.as_union c |> List.iter (fun (_,ty) -> of_param ty)
    else if Descr.mk_tagcomp c |> Ty.mk_descr |> Ty.is_empty |> not then
      found (TagOther tag))

(* Iterative deepening: increase the fuel until a witness is found.
   Terminates since every non-empty type has a finite inhabitant. *)
let of_ty ty =
  if Ty.is_empty ty then None
  else
    let rec aux fuel =
      match of_ty' fuel ty with
      | Some w -> Some w
      | None -> aux (fuel + 1)
    in
    aux 0

open Format
let rec pp fmt t =
  match t with
  | Other -> fprintf fmt "_"
  | Int z -> Z.pp_print fmt z
  | Enum e -> Enum.pp fmt e
  | EnumOther -> fprintf fmt "<enum>"
  | Tuple ts ->
    fprintf fmt "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp) ts
  | Record (bindings, tail) ->
    let fields =
      List.filter_map (function
        | (_, Absent) -> None
        | (l, Present w) -> Some (Some l, w)) bindings @
      (match tail with Absent -> [] | Present w -> [None, w])
    in
    let pp_label fmt = function
      | Some l -> Label.pp fmt l
      | None -> fprintf fmt "_"
    in
    let pp_field fmt (l, w) = fprintf fmt "%a = %a" pp_label l pp w in
    let pp_sep fmt () = fprintf fmt " ;@ " in
    begin match fields with
    | [] -> fprintf fmt "{}"
    | fs -> fprintf fmt "@[<hv 2>{ %a }@]" (pp_print_list ~pp_sep pp_field) fs
    end
  | Tag (tag, w) -> fprintf fmt "%a(%a)" Tag.pp tag pp w
  | TagOther tag -> fprintf fmt "%a(_)" Tag.pp tag
  | Arrow -> fprintf fmt "<fun>"
