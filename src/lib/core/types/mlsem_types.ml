
include Base

module Recording = Recording
module TVar = Tvar.TVar
module TVarSet = Tvar.TVarSet
module RVar = Tvar.RVar
module RVarSet = Tvar.RVarSet
module MVarSet = Tvar.MVarSet
type kind = Tvar.kind = KNoInfer | KInfer | KTemporary
module Row = struct
  include Tvar.Row
  let pp fmt _ = Format.fprintf fmt "_"
  let pp_raw fmt r = Sstt.Printer.print_row' fmt r
  let pp_full fmt r =
    let open Sstt.Printer in
    let bindings, tail = bindings r, tail r in
    let params = PEnv.printer_params () in
    let tail, fields, defs =
      match get_field' params (tail::List.map snd bindings) with
      | { main=tl::bindings ; defs } -> tl, bindings, defs
      | _ -> assert false
    in
    let bindings = List.combine (List.map fst bindings) fields in
    let ast = { main={ ty=Ty.any ; op=Record (bindings, tail) } ; defs } in
    print fmt ast
end
module Subst = struct
  include Tvar.Subst
  let pp fmt _ = Format.fprintf fmt "_"
  let pp_raw fmt t = Sstt.Printer.print_subst' fmt t
  let pp_full fmt s =
    let print_ty = Ty.pp in
    let print_row = Row.pp_full in
    let pp_binding1 fmt (v,ty) =
      Format.fprintf fmt "@,@[<hov>%a: %a@]" TVar.pp v print_ty ty
    in
    let pp_binding2 fmt (v,r) =
      Format.fprintf fmt "@,@[<hov>%a: %a@]" RVar.pp v print_row r
    in
    let pp_binding' fmt b =
      match b with
      | `T (v,ty) -> pp_binding1 fmt (v,ty)
      | `R (v,r) -> pp_binding2 fmt (v,r)
    in
    let b1 = bindings1 s |> List.map (fun b -> `T b) in
    let b2 = bindings2 s |> List.map (fun b -> `R b) in
    Format.fprintf fmt "@[<v 0>[@[<v 1>%a@]@,]@]"
      (Sstt.Prec.print_seq pp_binding' " ;") (b1@b2)

end
module TVOp = Tvar.TVOp

module GTy = GTy
module TyScheme = TyScheme
include Builder
