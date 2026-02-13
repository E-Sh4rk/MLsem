
include Base

module Recording = Recording
module Row = Tvar.Row
module TVar = Tvar.TVar
module TVarSet = Tvar.TVarSet
module RVar = Tvar.RVar
module RVarSet = Tvar.RVarSet
module MVarSet = Tvar.MVarSet
type kind = Tvar.kind = KNoInfer | KInfer | KTemporary
module Subst = struct
  include Tvar.Subst
  let pp fmt _ = Format.fprintf fmt "_"
  let pp_raw fmt t = Sstt.Printer.print_subst' fmt t
end
module TVOp = Tvar.TVOp

module GTy = GTy
module TyScheme = TyScheme
include Builder
