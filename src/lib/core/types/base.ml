let pparams_abs = ref []
let aliases = ref []

module Ty = struct
  type t = Sstt.Ty.t

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
  let printer_params () =
    let pparams' = { Sstt.Printer.empty_params with aliases = !aliases } in
    [ pparams ; pparams' ]@(!pparams_abs) |> Sstt.Printer.merge_params

  let pp fmt ty = Sstt.Printer.print_ty (printer_params ()) fmt ty

  let any = Sstt.Ty.any
  let empty = Sstt.Ty.empty

  let tt = Sstt.Extensions.Bools.bool true
  let ff = Sstt.Extensions.Bools.bool false
  let bool = Sstt.Extensions.Bools.any
  let int = Sstt.Intervals.any |> Sstt.Descr.mk_intervals |> Sstt.Ty.mk_descr
  let float = Sstt.Extensions.Floats.any
  let char = Sstt.Extensions.Chars.any
  let unit =
    let ty = Sstt.Descr.mk_tuple [] |> Sstt.Ty.mk_descr in
    register "()" ty ; ty
  let string = Sstt.Extensions.Strings.any

  let interval i1 i2 =
    Sstt.Intervals.Atom.mk i1 i2 |> Sstt.Intervals.mk
    |> Sstt.Descr.mk_intervals |> Sstt.Ty.mk_descr
  let char_interval c1 c2 = Sstt.Extensions.Chars.interval (c1, c2)
  let string_lit = Sstt.Extensions.Strings.str

  let cup = Sstt.Ty.cup
  let cap = Sstt.Ty.cap
  let diff = Sstt.Ty.diff
  let neg = Sstt.Ty.neg
  let conj = Sstt.Ty.conj
  let disj = Sstt.Ty.disj

  let is_empty = Sstt.Ty.is_empty
  let non_empty t = is_empty t |> not
  let is_any = Sstt.Ty.is_any
  let non_any t = is_any t |> not
  let leq = Sstt.Ty.leq
  let disjoint = Sstt.Ty.disjoint
  let equiv = Sstt.Ty.equiv

  let normalize = Sstt.Ty.factorize
  let simplify = Sstt.Transform.simplify
end

module Enum = struct
  type t = Sstt.Enums.Atom.t
  let pp = Sstt.Enums.Atom.pp
  let compare = Sstt.Enums.Atom.compare
  let define = Sstt.Enums.Atom.mk
  let typ t = t |> Sstt.Descr.mk_enum |> Sstt.Ty.mk_descr
  let any = Sstt.Enums.any |> Sstt.Descr.mk_enums |> Sstt.Ty.mk_descr
end

module Tag = struct
  type t = Sstt.TagComp.Tag.t
  let pp = Sstt.TagComp.Tag.pp
  let compare = Sstt.TagComp.Tag.compare
  let define = Sstt.TagComp.Tag.mk
  let mk tag ty = Sstt.Descr.mk_tag (tag, ty) |> Sstt.Ty.mk_descr
  let proj tag ty =
    Sstt.Ty.get_descr ty |> Sstt.Descr.get_tags |> Sstt.Tags.get tag
    |> Sstt.TagComp.as_atom |> snd
  let any = Sstt.Tags.any |> Sstt.Descr.mk_tags |> Sstt.Ty.mk_descr
end

module Abstract = struct
  type variance = Cov | Cav | Inv
  type t = Sstt.TagComp.Tag.t
  let define name vs =
    let aux = function
    | Cov -> Sstt.Extensions.Abstracts.Cov
    | Cav -> Sstt.Extensions.Abstracts.Contrav
    | Inv -> Sstt.Extensions.Abstracts.Inv
    in
    let (tag,printer) = Sstt.Extensions.Abstracts.define' name (List.map aux vs) in
    pparams_abs := printer::!pparams_abs ;
    tag
  let params abs =
    let aux = function
    | Sstt.Extensions.Abstracts.Cov -> Cov
    | Sstt.Extensions.Abstracts.Contrav -> Cav
    | Sstt.Extensions.Abstracts.Inv -> Inv
    in
    Sstt.Extensions.Abstracts.params_of abs |> List.map aux
  let mk = Sstt.Extensions.Abstracts.mk
  let any = Sstt.Extensions.Abstracts.mk_any
  let trans_tagcomp f c =
    match Sstt.Extensions.Abstracts.destruct c with
    | None -> c
    | Some (abs, dnf) ->
      let dnf = f (abs, dnf) in
      let mk_line (ps,ns) =
          let ps = ps |> List.map (fun lst -> mk abs lst) |> Ty.conj in
          let ns = ns |> List.map (fun lst -> mk abs lst) |> List.map Ty.neg |> Ty.conj in
          Ty.cap ps ns
      in
      dnf |> List.map mk_line |> Ty.disj
      |> Sstt.Ty.get_descr |> Sstt.Descr.get_tags |> Sstt.Tags.get abs
  let trans_tags f t = Sstt.Tags.map (trans_tagcomp f) t
  let trans_descr f d =
    let open Sstt.Descr in
    d |> components |> List.map (function
      | Intervals i -> Intervals i
      | Enums e -> Enums e
      | Tags t -> Tags (trans_tags f t)
      | Arrows a -> Arrows a
      | Tuples t -> Tuples t
      | Records r -> Records r
    ) |> of_components
  let trans_vdescr f = Sstt.VDescr.map (trans_descr f)
  let transform f = Sstt.Transform.transform (trans_vdescr f)
end

module Tuple = struct
  let mk ts = ts |> Sstt.Descr.mk_tuple |> Sstt.Ty.mk_descr
  let any = Sstt.Tuples.any |> Sstt.Descr.mk_tuples |> Sstt.Ty.mk_descr
  let any_n n = Sstt.TupleComp.any n |> Sstt.Descr.mk_tuplecomp |> Sstt.Ty.mk_descr

  let proj n i t =
    t |> Sstt.Ty.get_descr |> Sstt.Descr.get_tuples
    |> Sstt.Tuples.get n |> Sstt.Op.TupleComp.proj i

  let dnf n t =
    t |> Sstt.Ty.get_descr |> Sstt.Descr.get_tuples
    |> Sstt.Tuples.get n |> Sstt.Op.TupleComp.as_union

  let of_dnf n lst =
    let tc = Sstt.Op.TupleComp.of_union n lst in
    Sstt.Tuples.mk_comp tc |> Sstt.Descr.mk_tuples |> Sstt.Ty.mk_descr

  let decompose t =
    let (tcs, others) = t |> Sstt.Ty.get_descr |> Sstt.Descr.get_tuples
    |> Sstt.Tuples.components in
    let tcs = tcs |> List.map (fun tc -> Sstt.TupleComp.len tc, Sstt.Op.TupleComp.as_union tc) in
    tcs, others

  let recompose (tcs, others) =
    let tcs = tcs |> List.map (fun (n, dnf) -> Sstt.Op.TupleComp.of_union n dnf) in
    Sstt.Tuples.of_components (tcs, others) |> Sstt.Descr.mk_tuples |> Sstt.Ty.mk_descr
end

module Lst = struct
  let nil = Sstt.Extensions.Lists.nil
  let any = Sstt.Extensions.Lists.any
  let any_non_empty = Sstt.Extensions.Lists.any_non_empty
  let cons = Sstt.Extensions.Lists.cons
  let dnf = Sstt.Extensions.Lists.destruct
  let proj = Sstt.Extensions.Lists.proj
end

module Record = struct
  let labelmap = Hashtbl.create 256
  let to_label str =
    match Hashtbl.find_opt labelmap str with
    | Some lbl -> lbl
    | None ->
      let lbl = Sstt.Label.mk str in
      Hashtbl.add labelmap str lbl ; lbl
  let from_label lbl = Sstt.Label.name lbl

  let mk opened bindings =
    let bindings = bindings |>
      List.map (fun (str, (opt,ty)) -> (to_label str, (ty, opt))) |>
      Sstt.LabelMap.of_list in
    { Sstt.Records.Atom.opened ; bindings }
    |> Sstt.Descr.mk_record |> Sstt.Ty.mk_descr

  let any =
    Sstt.Records.any |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr
  let empty_closed = mk false []
  let any_with l = mk true [l, (false, Ty.any)]
  let any_without l = mk true [l, (true, Ty.empty)]

  let dnf t =
    t |> Sstt.Ty.get_descr |> Sstt.Descr.get_records
    |> Sstt.Op.Records.as_union |> List.map (fun a ->
      let opened = a.Sstt.Records.Atom.opened in
      let bindings = a.bindings |> Sstt.LabelMap.bindings |>
        List.map (fun (lbl, (ty,opt)) -> (from_label lbl, (opt,ty))) in
      bindings, opened
    )
  let of_dnf lst =
    let lst = lst |> List.map (fun (bs, opened) ->
      let bindings = bs |>
        List.map (fun (str, (opt,ty)) -> (to_label str, (ty, opt))) |> Sstt.LabelMap.of_list
      in
      { Sstt.Records.Atom.opened ; bindings }
      )
    in
    Sstt.Op.Records.of_union lst |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr

  let proj t field =
    t |> Sstt.Ty.get_descr |> Sstt.Descr.get_records
    |> Sstt.Op.Records.proj (to_label field) |> fst

  let merge t1 t2 =
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
end

module Arrow = struct
  let mk t1 t2 =
    Sstt.Descr.mk_arrow (t1,t2) |> Sstt.Ty.mk_descr

  let any = Sstt.Arrows.any |> Sstt.Descr.mk_arrows |> Sstt.Ty.mk_descr

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
end
