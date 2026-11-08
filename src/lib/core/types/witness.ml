open Sstt

type t =
  | Other
  | Int of Z.t
  | Enum of Enum.t
  | Tuple of t list
  | Record of (Label.t * tf) list * tf
  | Tag of Tag.t * t
  | TagOther of Tag.t
  | Arrow
and tf =
  | Absent
  | Present of t

let enum_default = Enum.mk "abc"

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
  | false, enums ->
    assert (List.exists (Enum.equal enum_default) enums |> not) ;
    found (Enum enum_default)
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

(* Conversion to types, used to print values of extension types
   (booleans, strings, lists, ...) with the printer of the extension. *)

let rec to_ty t =
  match t with
  | Other -> Ty.any
  | Int z -> Intervals.Atom.mk_singl z |> Descr.mk_interval |> Ty.mk_descr
  | Enum e -> Descr.mk_enum e |> Ty.mk_descr
  | Tuple ws -> List.map to_ty ws |> Descr.mk_tuple |> Ty.mk_descr
  | Record (bindings, tail) ->
    let to_oty = function
      | Absent -> Ty.O.absent
      | Present w -> Ty.O.mk (to_ty w, false)
    in
    let bindings =
      bindings |> List.map (fun (l, f) -> (l, to_oty f))
      |> Op.Records.Atom.LabelMap.of_list
    in
    { Op.Records.Atom.bindings ; tail=to_oty tail }
    |> Op.Records.of_atom |> Descr.mk_records |> Ty.mk_descr
  | Tag (tag, w) -> Descr.mk_tag (tag, to_ty w) |> Ty.mk_descr
  | TagOther tag when Extensions.Abstracts.is_abstract tag ->
    Extensions.Abstracts.mk_any tag
  | TagOther tag -> TagComp.any tag |> Descr.mk_tagcomp |> Ty.mk_descr
  | Arrow -> Arrows.any |> Descr.mk_arrows |> Ty.mk_descr

(* Whether the witness denotes a single value
   (as opposed to an unspecified value of some type) *)
let rec is_exact t =
  match t with
  | Other | TagOther _ | Arrow -> false
  | Int _ | Enum _ -> true
  | Tuple ws -> List.for_all is_exact ws
  | Record (bindings, tail) ->
    List.for_all (fun (_, f) -> is_exact_field f) bindings && is_exact_field tail
  | Tag (_, w) -> is_exact w
and is_exact_field = function
  | Absent -> true
  | Present w -> is_exact w

let is_extension tag =
  let params = Base.PrinterCfg.printer_params () in
  List.exists (fun (tag', _) -> Tag.equal tag tag') params.Printer.extensions

let pp_as_ty fmt t =
  let open Format in
  let pp_ty fmt t =
    Printer.get (Base.PrinterCfg.printer_params ()) (to_ty t)
    |> Base.PrinterCfg.print fmt
  in
  if is_exact t then pp_ty fmt t
  else fprintf fmt "<%a>" pp_ty t

(* Custom printers for tagged values *)

type printer = Format.formatter -> t -> unit

(* Strings are encoded as a tag applied to enums, one per string literal.
   [enum_default] is not one of them: it stands for a literal
   not mentioned in the type. *)
let pp_string fmt w =
  match w with
  | Enum e when Enum.equal e enum_default -> Format.fprintf fmt "%S" "abc"
  | w -> pp_as_ty fmt (Tag (Extensions.Strings.tag, w))

(* Forward reference to [pp], for printers of values containing values *)
let pp_fwd : printer ref = ref (fun _ _ -> assert false)

(* Lists are encoded as a tag applied to either [()] (the empty list)
   or a pair of the head and the tail. *)
let pp_list fmt w =
  let open Format in
  let pp fmt w = !pp_fwd fmt w in
  let rec destruct acc w =
    match w with
    | Tuple [] -> List.rev acc, None
    | Tuple [hd ; Tag (tag, tl)] when Tag.equal tag Extensions.Lists.tag ->
      destruct (hd::acc) tl
    | Tuple [hd ; tl] -> List.rev (hd::acc), Some tl
    | w -> List.rev acc, Some w
  in
  match destruct [] w with
  | elts, None ->
    fprintf fmt "@[<hv 1>[%a]@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") pp) elts
  | elts, Some tl ->
    (* Not a proper list (should not happen for well-formed witnesses) *)
    let pp_elt fmt w = fprintf fmt "%a ::@ " pp w in
    fprintf fmt "@[<hv>%a%a@]" (pp_print_list ~pp_sep:(fun _ () -> ()) pp_elt) elts pp tl

(* Floats are encoded as a tag applied to an enum representing their kind:
   we print a representative float of that kind. *)
let float_kinds =
  let open Extensions.Floats in
  let enum_of_kind k =
    flt k |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
    |> Op.TagComp.as_atom |> snd
    |> Ty.get_descr |> Descr.get_enums |> Enums.destruct |> snd |> List.hd
  in
  lazy ([ Ninf, "-infinity" ; Neg, "-1.0" ; Nzero, "-0.0" ; Pzero, "0.0" ;
          Pos, "1.0" ; Pinf, "infinity" ; Nan, "nan" ]
        |> List.map (fun (k, str) -> enum_of_kind k, str))

let pp_float fmt w =
  let kind = match w with
    | Enum e ->
      Lazy.force float_kinds |> List.find_map
        (fun (e', str) -> if Enum.equal e e' then Some str else None)
    | _ -> None
  in
  match kind with
  | Some str -> Format.pp_print_string fmt str
  | None -> pp_as_ty fmt (Tag (Extensions.Floats.tag, w))

let builtin_printers = [
  Extensions.Strings.tag, pp_string ;
  Extensions.Floats.tag, pp_float ;
  Extensions.Lists.tag, pp_list
]
let extra_printers = ref []

let add_printer_param p = extra_printers := p :: !extra_printers
let set_printer_params ps = extra_printers := ps

let find_printer tag =
  let find = List.find_map (fun (tag', p) -> if Tag.equal tag tag' then Some p else None) in
  match find !extra_printers with
  | Some p -> Some p
  | None -> find builtin_printers

open Format
let rec pp fmt t =
  match t with
  | Tag (tag, w) when Option.is_some (find_printer tag) ->
    (Option.get (find_printer tag)) fmt w
  | Tag (tag, _) | TagOther tag when is_extension tag -> pp_as_ty fmt t
  | Other -> fprintf fmt "_"
  | Int z -> Z.pp_print fmt z
  | Enum e -> Enum.pp fmt e
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

let () = pp_fwd := pp
