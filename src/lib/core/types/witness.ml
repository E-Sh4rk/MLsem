open Sstt

type t =
  | Default
  | Int of Z.t
  | Enum of Enum.t
  | EnumDefault
  | Tuple of t list
  | TupleDefault
  | Record of (Label.t * tf) list * tf
  | Tag of Tag.t * t
  | TagOpaque of Tag.t
  | TagDefault
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
   content.
   Parts of a type that are infinite unions of components (e.g. all the tuples
   of arities not listed) are not explored: they give a default witness, as
   they may include components that do not correspond to values of the
   language. *)
let rec of_ty' fuel ty =
  if fuel < 0 then None
  else try Ty.def ty |> of_vdescr fuel ; None with Found t -> Some t
and of_vdescr fuel vd =
  let lines = VDescr.dnf vd |> List.map (fun (_,_,d) -> d) in
  (* Default witnesses are only considered if no other witness
     is found, in any line of the DNF *)
  lines |> List.iter (of_descr fuel) ;
  lines |> List.iter of_descr_fallback
and of_descr fuel d =
  (* A negative descriptor (i.e. containing all the components not listed)
     gives a default witness *)
  if Descr.get_others d then found Default ;
  Descr.get_intervals d |> of_intervals ;
  Descr.get_enums d |> of_finite_enums ;
  let fuel = fuel - 1 in
  Descr.get_tuples d |> of_tuples fuel ;
  Descr.get_records d |> of_records fuel ;
  Descr.get_tags d |> of_tags fuel
and of_descr_fallback d =
  Descr.get_enums d |> of_infinite_enums ;
  Descr.get_tuples d |> of_infinite_tuples ;
  Descr.get_tags d |> of_infinite_tags ;
  Descr.get_arrows d |> of_arrows
(* [Enums.destruct] returns [(true, enums)] for exactly [enums],
   and [(false, enums)] for all constants but [enums]. *)
and of_finite_enums e =
  match Enums.destruct e with
  | true, e::_ -> found (Enum e)
  | _, _ -> ()
and of_infinite_enums e =
  match Enums.destruct e with
  | false, _ -> found EnumDefault
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
and of_tuples fuel t =
  let comps, _ = Tuples.components t in
  comps |> List.iter (fun c ->
    Op.TupleComp.as_union c |> List.iter (fun atom ->
      match List.map (of_ty' fuel) atom |> all_some with
      | Some ws -> found (Tuple ws)
      | None -> ()))
and of_infinite_tuples t =
  (* Tuples of any arity not listed are included *)
  if Tuples.components t |> snd then found TupleDefault
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
      found (TagOpaque tag))
and of_infinite_tags t =
  (* Tags not listed are included *)
  if Tags.components t |> snd then found TagDefault

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

(* Custom printers for tagged values *)

type printer = Format.formatter -> t -> unit

(* Forward reference to [pp], for printers of values containing values *)
let pp_fwd : printer ref = ref (fun _ _ -> assert false)

(* Default printing of a tagged value (used by printers on unexpected content) *)
let pp_tag tag fmt w = Format.fprintf fmt "%a(%a)" Tag.pp tag !pp_fwd w

(* Printer for the extension [tag], given [f] which interprets the value
   of the extension encoded by the tagged type. The content of the tag
   must be an enum constant or an integer. *)
let pp_extension tag destruct f fmt w =
  let content = match w with
    | Enum e -> Some (Descr.mk_enum e |> Ty.mk_descr)
    | Int z -> Some (Intervals.Atom.mk_singl z |> Descr.mk_interval |> Ty.mk_descr)
    | _ -> None
  in
  let str = match content with
    | None -> None
    | Some ty ->
      try Descr.mk_tag (tag, ty) |> Ty.mk_descr |> destruct |> f
      with Invalid_argument _ -> None
  in
  match str with
  | Some str -> Format.pp_print_string fmt str
  | None -> pp_tag tag fmt w

let pp_bool =
  let open Extensions.Bools in
  pp_extension tag destruct (fun t ->
    match components t with
    | [b] -> Some (string_of_bool b)
    | _ -> None)

(* Floats are abstracted by their kind: we print a representative float *)
let pp_float =
  let open Extensions.Floats in
  pp_extension tag destruct (fun t ->
    match components t with
    | [Ninf] -> Some "-infinity" | [Neg] -> Some "-1.0"
    | [Nzero] -> Some "-0.0" | [Pzero] -> Some "0.0"
    | [Pos] -> Some "1.0" | [Pinf] -> Some "infinity"
    | [Nan] -> Some "nan"
    | _ -> None)

let pp_char =
  let open Extensions.Chars in
  pp_extension tag destruct (function
    | [(c1, c2)] when Char.equal c1 c2 -> Some (Format.sprintf "%C" c1)
    | _ -> None)

(* [EnumDefault] stands for a literal not mentioned in the type *)
let pp_string fmt w =
  let open Extensions.Strings in
  match w with
  | EnumDefault -> Format.fprintf fmt "%S" "abc"
  | w ->
    pp_extension tag destruct (function
      | (true, [str]) -> Some (Format.sprintf "%S" str)
      | _ -> None) fmt w

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

type printer_param =
| Tagged of Tag.t * printer
| Opaque of Tag.t * string

(* Printers that are not removed by [set_printer_params] *)
let builtin_printers = ref [
  Tagged (Extensions.Bools.tag, pp_bool) ;
  Tagged (Extensions.Floats.tag, pp_float) ;
  Tagged (Extensions.Chars.tag, pp_char) ;
  Tagged (Extensions.Strings.tag, pp_string) ;
  Tagged (Extensions.Lists.tag, pp_list)
]
let extra_printers = ref []

let add_builtin_printer_param p = builtin_printers := p :: !builtin_printers
let add_printer_param p = extra_printers := p :: !extra_printers
let set_printer_params ps = extra_printers := ps

(* Extra printers have priority over builtin ones *)
let find_param f = List.find_map f (!extra_printers @ !builtin_printers)
let find_printer tag =
  find_param (function
    | Tagged (tag', p) when Tag.equal tag tag' -> Some p | _ -> None)
let find_opaque tag =
  find_param (function
    | Opaque (tag', str) when Tag.equal tag tag' -> Some str | _ -> None)

(* Representatives for default witnesses. They are arbitrary, but chosen to be
   unlikely to be excluded by the type (they are only used for error reports). *)
let default_pp = ref "_"
let enum_default_pp = ref "Abc"
let tuple_default_pp = ref "(_, _, _, _, _)"
let tag_default_pp = ref "Abd(_)"

let set_default_printer str = default_pp := str
let set_enum_default_printer str = enum_default_pp := str
let set_tuple_default_printer str = tuple_default_pp := str
let set_tag_default_printer str = tag_default_pp := str

open Format
let rec pp fmt t =
  match t with
  | Tag (tag, w) ->
    begin match find_printer tag with
    | Some p -> p fmt w
    | None -> fprintf fmt "%a(%a)" Tag.pp tag pp w
    end
  | Default -> pp_print_string fmt !default_pp
  | Int z -> Z.pp_print fmt z
  | Enum e -> Enum.pp fmt e
  | EnumDefault -> pp_print_string fmt !enum_default_pp
  | TupleDefault -> pp_print_string fmt !tuple_default_pp
  | TagDefault -> pp_print_string fmt !tag_default_pp
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
  | TagOpaque tag ->
    begin match find_opaque tag with
    | Some str -> pp_print_string fmt str
    | None -> fprintf fmt "%a(_)" Tag.pp tag
    end
  | Arrow -> fprintf fmt "<fun>"

let () = pp_fwd := pp
