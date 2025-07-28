
module LabelSet = Sstt.LabelSet
module LabelMap = Sstt.LabelMap

type typ = Sstt.Ty.t

let aliases = ref []
let register str ty =
  aliases := (ty,str)::!aliases

let pparams =
  [
    Sstt.Extensions.Bools.printer_params' ;
    Sstt.Extensions.Floats.printer_params' ;
    Sstt.Extensions.Strings.printer_params' ;
    Sstt.Extensions.Lists.printer_params' ;
    Sstt.Extensions.Chars.printer_params'
  ] |> Sstt.Printer.merge_params
let pparams_abs = ref []
let printer_params () =
  let pparams' = { Sstt.Printer.empty_params with aliases = !aliases } in
  [ pparams ; pparams' ]@(!pparams_abs) |> Sstt.Printer.merge_params

let pp_typ = Sstt.Printer.print_ty (printer_params ())

let any = Sstt.Ty.any
let empty = Sstt.Ty.empty
let normalize = Sstt.Ty.factorize
let simplify = Sstt.Transform.simplify

(* ----- *)

let is_empty = Sstt.Ty.is_empty
let non_empty t = is_empty t |> not
let is_any = Sstt.Ty.is_any
let non_any t = is_any t |> not
let subtype = Sstt.Ty.leq
let disjoint = Sstt.Ty.disjoint
let equiv = Sstt.Ty.equiv

let cup t1 t2 = Sstt.Ty.cup t1 t2
let cap t1 t2 = Sstt.Ty.cap t1 t2
let diff = Sstt.Ty.diff
let neg = Sstt.Ty.neg
let conj = Sstt.Ty.conj
let disj = Sstt.Ty.disj

(* ----- *)

let labelmap = Hashtbl.create 256
let to_label str =
  match Hashtbl.find_opt labelmap str with
  | Some lbl -> lbl
  | None ->
    let lbl = Sstt.Label.mk str in
    Hashtbl.add labelmap str lbl ; lbl
let from_label lbl = Sstt.Label.name lbl

(* ----- *)

type enum = Sstt.Enums.Atom.t
let pp_enum = Sstt.Enums.Atom.pp
let compare_enum = Sstt.Enums.Atom.compare
let define_enum name = name |> Sstt.Enums.Atom.mk
let mk_enum enum = enum |> Sstt.Descr.mk_enum |> Sstt.Ty.mk_descr
let enum_any = Sstt.Enums.any |> Sstt.Descr.mk_enums |> Sstt.Ty.mk_descr

type tag = Sstt.TagComp.Tag.t
let pp_tag = Sstt.TagComp.Tag.pp
let compare_tag = Sstt.TagComp.Tag.compare
let unsafe_to_tag t = t
let define_tag name = name |> Sstt.TagComp.Tag.mk
let mk_tag tag ty = Sstt.Descr.mk_tag (tag, ty) |> Sstt.Ty.mk_descr
let destruct_tag tag ty =
  Sstt.Ty.get_descr ty |> Sstt.Descr.get_tags |> Sstt.Tags.get tag
  |> Sstt.TagComp.as_atom |> snd
let tag_any = Sstt.Tags.any |> Sstt.Descr.mk_tags |> Sstt.Ty.mk_descr

type variance = Cov | Cav | Inv
type abstract = Sstt.TagComp.Tag.t
let unsafe_to_abstract t = t
let define_abstract name vs =
  let aux = function
  | Cov -> Sstt.Extensions.Abstracts.Cov
  | Cav -> Sstt.Extensions.Abstracts.Contrav
  | Inv -> Sstt.Extensions.Abstracts.Inv
  in
  let (tag,printer) = Sstt.Extensions.Abstracts.define' name (List.map aux vs) in
  pparams_abs := printer::!pparams_abs ;
  tag
let params_of_abstract abs =
  let aux = function
  | Sstt.Extensions.Abstracts.Cov -> Cov
  | Sstt.Extensions.Abstracts.Contrav -> Cav
  | Sstt.Extensions.Abstracts.Inv -> Inv
  in
  Sstt.Extensions.Abstracts.params_of abs |> List.map aux
let mk_abstract = Sstt.Extensions.Abstracts.mk
let mk_abstract_any = Sstt.Extensions.Abstracts.mk_any

let true_typ = Sstt.Extensions.Bools.bool true
let false_typ = Sstt.Extensions.Bools.bool false
let bool_typ = Sstt.Extensions.Bools.any
let int_typ = Sstt.Intervals.any |> Sstt.Descr.mk_intervals |> Sstt.Ty.mk_descr
let float_typ = Sstt.Extensions.Floats.any
let char_typ = Sstt.Extensions.Chars.any
let unit_typ =
  let ty = Sstt.Descr.mk_tuple [] |> Sstt.Ty.mk_descr in
  register "()" ty ; ty
let string_typ = Sstt.Extensions.Strings.any

let interval i1 i2 =
  Sstt.Intervals.Atom.mk i1 i2 |> Sstt.Intervals.mk
  |> Sstt.Descr.mk_intervals |> Sstt.Ty.mk_descr
    
let char_interval c1 c2 = Sstt.Extensions.Chars.interval (c1, c2)

let single_string str = Sstt.Extensions.Strings.str str

let mk_tuple ts = ts |> Sstt.Descr.mk_tuple |> Sstt.Ty.mk_descr
let tuple_any = Sstt.Tuples.any |> Sstt.Descr.mk_tuples |> Sstt.Ty.mk_descr
let tuple_n n = Sstt.TupleComp.any n |> Sstt.Descr.mk_tuplecomp |> Sstt.Ty.mk_descr

let pi n i t =
  t |> Sstt.Ty.get_descr |> Sstt.Descr.get_tuples
  |> Sstt.Tuples.get n |> Sstt.Op.TupleComp.proj i

let tuple_dnf n t =
  t |> Sstt.Ty.get_descr |> Sstt.Descr.get_tuples
  |> Sstt.Tuples.get n |> Sstt.Op.TupleComp.as_union

let tuple_of_dnf n lst =
  let tc = Sstt.Op.TupleComp.of_union n lst in
  Sstt.Tuples.mk_comp tc |> Sstt.Descr.mk_tuples |> Sstt.Ty.mk_descr

let tuple_decompose t =
  let (tcs, others) = t |> Sstt.Ty.get_descr |> Sstt.Descr.get_tuples
  |> Sstt.Tuples.components in
  let tcs = tcs |> List.map (fun tc -> Sstt.TupleComp.len tc, Sstt.Op.TupleComp.as_union tc) in
  tcs, others

let tuple_recompose (tcs, others) =
  let tcs = tcs |> List.map (fun (n, dnf) -> Sstt.Op.TupleComp.of_union n dnf) in
  Sstt.Tuples.of_components (tcs, others) |> Sstt.Descr.mk_tuples |> Sstt.Ty.mk_descr

let nil_typ = Sstt.Extensions.Lists.nil
let list_typ = Sstt.Extensions.Lists.any
let non_empty_list_typ = Sstt.Extensions.Lists.any_non_empty
let mk_cons = Sstt.Extensions.Lists.cons
let cons_dnf = Sstt.Extensions.Lists.destruct
let destruct_list = Sstt.Extensions.Lists.destruct'

let mk_record opened bindings =
  let bindings = bindings |>
    List.map (fun (str, (opt,ty)) -> (to_label str, (ty, opt))) |>
    Sstt.LabelMap.of_list in
  { Sstt.Records.Atom.opened ; bindings }
  |> Sstt.Descr.mk_record |> Sstt.Ty.mk_descr

let record_any_with l = mk_record true [l, (false, any)]

let record_any_without l = mk_record true [l, (true, empty)]

let record_dnf t =
  t |> Sstt.Ty.get_descr |> Sstt.Descr.get_records
  |> Sstt.Op.Records.as_union |> List.map (fun a ->
    let opened = a.Sstt.Records.Atom.opened in
    let bindings = a.bindings |> Sstt.LabelMap.bindings |>
      List.map (fun (lbl, (ty,opt)) -> (from_label lbl, (opt,ty))) in
    bindings, opened
  )
let record_of_dnf lst =
  let lst = lst |> List.map (fun (bs, opened) ->
    let bindings = bs |>
      List.map (fun (str, (opt,ty)) -> (to_label str, (ty, opt))) |> Sstt.LabelMap.of_list
    in
    { Sstt.Records.Atom.opened ; bindings }
    )
  in
  Sstt.Op.Records.of_union lst |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr

let record_any =
  Sstt.Records.any |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr

let empty_closed_record = mk_record false []

let get_field t field =
  t |> Sstt.Ty.get_descr |> Sstt.Descr.get_records
  |> Sstt.Op.Records.proj (to_label field)

let get_field t field = get_field t field |> fst

let merge_records t1 t2 =
  try
    let r1 = Sstt.Ty.get_descr t1 |> Sstt.Descr.get_records |> Sstt.Op.Records.approx in
    let r2 = Sstt.Ty.get_descr t2 |> Sstt.Descr.get_records |> Sstt.Op.Records.approx in
    Sstt.Op.Records.merge r1 r2 |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr
  with Sstt.Op.EmptyAtom -> Sstt.Ty.empty

let remove_field t field =
  try
    let r = Sstt.Ty.get_descr t |> Sstt.Descr.get_records |> Sstt.Op.Records.approx in
    Sstt.Op.Records.remove r (to_label field) |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr
  with Sstt.Op.EmptyAtom -> Sstt.Ty.empty

let mk_arrow t1 t2 =
  Sstt.Descr.mk_arrow (t1,t2) |> Sstt.Ty.mk_descr

let arrow_any = Sstt.Arrows.any |> Sstt.Descr.mk_arrows |> Sstt.Ty.mk_descr

let domain t =
  let a = Sstt.Ty.get_descr t |> Sstt.Descr.get_arrows in
  Sstt.Op.Arrows.dom a

let apply t args =
  let a = Sstt.Ty.get_descr t |> Sstt.Descr.get_arrows in
  Sstt.Op.Arrows.apply a args

let dnf t =
  let a = Sstt.Ty.get_descr t |> Sstt.Descr.get_arrows in
  Sstt.Arrows.dnf a |> Sstt.Arrows.Dnf.simplify |> List.map (fun (ps,_,_) -> ps)

let of_dnf lst =
  lst |> List.map (fun ps -> (ps,[],true)) |> Sstt.Arrows.of_dnf
  |> Sstt.Descr.mk_arrows |> Sstt.Ty.mk_descr
