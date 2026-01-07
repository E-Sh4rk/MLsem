
open Effect

module PEnv = struct
  let pparams = ref (
    [
      Sstt.Extensions.Bools.printer_params ;
      Sstt.Extensions.Floats.printer_params ;
      Sstt.Extensions.Strings.printer_params ;
      Sstt.Extensions.Lists.printer_params ;
      Sstt.Extensions.Chars.printer_params
    ] |> Sstt.Printer.merge_params
  )

  type gstring (* gap string *) = string (* name *)
                                * (string * Sstt.Var.t) list (* gaps (type variables) *)
  type t = { aliases: (Sstt.Ty.t * string) list ;
             paliases: (Sstt.Ty.t * gstring) list }

  type _ Effect.t += Update: t -> unit Effect.t
  type _ Effect.t += Get: t Effect.t

  let add_printer_param p = pparams := Sstt.Printer.merge_params [!pparams ; p]
  let printer_params' s =
    let penv = perform Get in
    let paliases =
      penv.paliases |> List.map (fun (ty, (str,holes)) ->
          let ty = Sstt.Subst.apply s ty in
          let replace acc (str,v) =
            Str.global_replace (Str.regexp str)
              (Sstt.Subst.find1 s v |> Format.asprintf "%a" Sstt.Printer.print_ty') acc
          in
          let str = List.fold_left replace str holes in
          ty, str
        )
    in
    let aliases = penv.aliases in
    let pparams' = { Sstt.Printer.empty_params with aliases=aliases@paliases } in
    Sstt.Printer.merge_params [ !pparams ; pparams' ]
  let printer_params () = printer_params' Sstt.Subst.identity

  let empty = { aliases = [] ; paliases = [] }

  let merge penv1 penv2 =
    let add_alias a (ty, str) =
      let a = a
      |> List.filter (fun (_,str') -> String.equal str str' |> not)
      |> List.filter (fun (ty',_) -> Sstt.Ty.equiv ty ty' |> not)
      in
      (ty,str)::a
    in
    let add_palias pa (ty, gstr) =
      let pa = pa |> List.filter (fun (ty',_) -> Sstt.Ty.equiv ty ty' |> not) in
      (ty, gstr)::pa
    in
    let aliases = List.fold_left add_alias penv1.aliases penv2.aliases in
    let paliases = List.fold_left add_palias penv1.paliases penv2.paliases in
    { aliases ; paliases }
  let merge' penvs = List.fold_left merge empty penvs

  let sequential_handler (t:t) f a =
    let open Effect.Deep in
    let t = ref t in
    match f a with
    | x -> x, !t
    | effect Get, k -> continue k !t
    | effect Update t', k -> t := merge !t t' ; continue k ()

  let register str ty =
    perform (Update { empty with aliases=[ty,str] })

  let register_parametrized name ps ty =
    let module StrMap = Map.Make(String) in
    let sep,prio,_ = Sstt.Prec.varop_info Sstt.Prec.Tuple in
    let hmap = ref StrMap.empty in
    let id = ref 0 in
    let new_var v =
      let name = "{{"^(string_of_int !id)^"}}" in
      id := !id+1 ; hmap := StrMap.add name v !hmap ;
      name
    in
    let pparam = printer_params () in
    let pp_param fmt ty =
      let t = Sstt.Printer.get ~factorize:false pparam ty in
      let rename_vars (d:Sstt.Printer.descr) =
        match d.op with
        | Sstt.Printer.Var v -> Sstt.Printer.Var (Sstt.Var.mk (new_var v))
        | o -> o
      in
      if List.is_empty t.defs then
        let d = t.main |> Sstt.Printer.map_descr rename_vars in
        Sstt.Printer.print_descr_ctx prio Sstt.Prec.NoAssoc fmt d
      else
        let t = t |> Sstt.Printer.map rename_vars in
        Format.fprintf fmt "(%a)" Sstt.Printer.print t
    in
    let str = Format.asprintf "%s(%a)" name (Sstt.Prec.print_seq pp_param sep) ps in
    perform (Update { empty with paliases=[ty, (str, StrMap.bindings !hmap)] })
end

module Ty = struct
  type t = Sstt.Ty.t
  
  let pp fmt ty = Sstt.Printer.print_ty (PEnv.printer_params ()) fmt ty
  let pp' s fmt ty =
    Sstt.Printer.print_ty (PEnv.printer_params' s) fmt (Sstt.Subst.apply s ty)
  let pp_raw fmt ty =
    let t = Sstt.Printer.get ~factorize:false Sstt.Printer.empty_params ty in
    Sstt.Printer.print fmt t
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
    PEnv.add_printer_param { Sstt.Printer.empty_params with aliases=[ty, "()"] } ; ty
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
  let simplify ty = Sstt.Transform.simplify ty
end

module FTy = struct
  include Sstt.Ty.F

  let of_oty (t,o) = Sstt.Ty.F.mk_descr (t,o)
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
  type t = Sstt.Tag.t
  let pp = Sstt.Tag.pp
  let compare = Sstt.Tag.compare
  let define = Sstt.Tag.mk
  let mk tag ty = Sstt.Descr.mk_tag (tag, ty) |> Sstt.Ty.mk_descr
  let proj tag ty =
    Sstt.Ty.get_descr ty |> Sstt.Descr.get_tags |> Sstt.Tags.get tag
    |> Sstt.Op.TagComp.as_atom |> snd
  let any = Sstt.Tags.any |> Sstt.Descr.mk_tags |> Sstt.Ty.mk_descr
end

module Abstract = struct
  type t = Sstt.Tag.t
  let define name n =
    let vs = List.init n (fun _ -> Sstt.Extensions.Abstracts.Inv) in
    let tag = Sstt.Extensions.Abstracts.define name vs in
    let printer = Sstt.Extensions.Abstracts.printer_params tag in
    PEnv.add_printer_param printer ; tag
  let arity = Sstt.Extensions.Abstracts.arity
  let mk = Sstt.Extensions.Abstracts.mk
  let any = Sstt.Extensions.Abstracts.mk_any
  let dnf tag ty =
    Sstt.Extensions.Abstracts.destruct tag ty
    |> List.map fst

  let trans_tagcomp f c =
    let ty = Sstt.Tags.mk_comp c |> Sstt.Descr.mk_tags |> Sstt.Ty.mk_descr in
    let abs = Sstt.TagComp.tag c in
    match Sstt.Extensions.Abstracts.destruct abs ty with
    | exception _ -> c
    | dnf ->
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
    let (pos, comps) = destruct d in
    let comps = comps |> List.map (function
      | Intervals i -> Intervals i
      | Enums e -> Enums e
      | Tags t -> Tags (trans_tags f t)
      | Arrows a -> Arrows a
      | Tuples t -> Tuples t
      | Records r -> Records r
    ) in
    construct (pos, comps)
  let trans_vdescr f = Sstt.VDescr.map (trans_descr f)
  let transform f ty =
    let open Sstt in
    let has_abs = ref false in
    let _ = ty |> Ty.nodes |> List.map (fun ty ->
      Ty.def ty |> VDescr.map (fun d ->
        let (tags,_) = Descr.get_tags d |> Tags.components in
        if List.exists (fun tc -> TagComp.tag tc |> Extensions.Abstracts.is_abstract) tags
        then has_abs := true ;
        d
      )) in
    if !has_abs then Transform.transform (trans_vdescr f) ty else ty
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
  type oty = Ty.t*bool

  let labelmap = Hashtbl.create 256
  let to_label str =
    match Hashtbl.find_opt labelmap str with
    | Some lbl -> lbl
    | None ->
      let lbl = Sstt.Label.mk str in
      Hashtbl.add labelmap str lbl ; lbl
  let from_label lbl = Sstt.Label.name lbl

  module LabelMap = Sstt.Op.Records.Atom.LabelMap
  let mk tail bindings =
    let bindings = bindings |>
      List.map (fun (str, (ty, opt)) -> (to_label str, (ty, opt))) |>
      LabelMap.of_list in
    { Sstt.Op.Records.Atom.tail ; bindings } |> Sstt.Op.Records.of_atom
    |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr
  let mk_open = mk (Ty.any, true)
  let mk_closed = mk (Ty.empty, true)

  let mk' tail bindings =
    let bindings = bindings |>
      List.map (fun (str, f) -> (to_label str, f)) |>
      Sstt.LabelMap.of_list in
    { Sstt.Records.Atom.tail ; bindings } |> Sstt.Records.mk
    |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr

  let any =
    Sstt.Records.any |> Sstt.Descr.mk_records |> Sstt.Ty.mk_descr
  let any_with l = mk (Ty.any, true) [l, (Ty.any, false)]
  let any_without l = mk (Ty.any, true) [l, (Ty.empty, true)]

  let dnf t =
    t |> Sstt.Ty.get_descr |> Sstt.Descr.get_records
    |> Sstt.Op.Records.as_union |> List.map (fun a ->
      let bindings = a.Sstt.Op.Records.Atom.bindings |> LabelMap.bindings |>
        List.map (fun (lbl, (ty,opt)) -> (from_label lbl, (ty,opt))) in
      bindings, a.tail
    )
  let of_dnf lst =
    let lst = lst |> List.map (fun (bs, tail) ->
        let bindings = bs |>
          List.map (fun (str, (ty,opt)) -> (to_label str, (ty,opt))) |> LabelMap.of_list
        in
        { Sstt.Op.Records.Atom.tail ; bindings }
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
    Sstt.Arrows.dnf a |> List.map fst

  let of_dnf lst =
    lst |> List.map (fun ps -> (ps,[])) |> Sstt.Arrows.of_dnf
    |> Sstt.Descr.mk_arrows |> Sstt.Ty.mk_descr
end
