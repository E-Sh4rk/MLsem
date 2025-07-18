open Base
open Tvar

module GTy = struct
  type t = typ * bool
  let dyn = (empty, true)
  let static = (any, false)
  let top = (any, true)
  let bot = (empty, false)
  let mk ty b = (ty, b)
  let mk_static ty = mk ty false
  let dyn_comp (_,b) = b
  let static_comp (ty,_) = ty
  let components = Fun.id
  let cap (ty1,b1) (ty2,b2) = (cap ty1 ty2, b1 && b2)
  let cup (ty1,b1) (ty2,b2) = (cup ty1 ty2, b1 || b2)

  let fv (ty,_) = vars ty
  let substitute s (ty,b) = (Subst.apply s ty,b)

  let is_top (ty, b) = b && (is_any ty)
  let is_bot (ty, b) = (not b) && (is_empty ty)
  let leq_static (ty1, _) (ty2, _) = subtype ty1 ty2
  let equiv_static (ty1, _) (ty2, _) = equiv ty1 ty2
  let leq (ty1,b1) (ty2,b2) = (b2 || not b1) && (subtype ty1 ty2)
  let equiv (ty1,b1) (ty2,b2) = (b1 = b2) && (equiv ty1 ty2)

  let simplify (ty,b) = (Additions.simplify_typ ty, b)
  let map f (ty, b) = (f ty, b)

  let pp fmt (ty,b) =
    if b then
      if is_empty ty then Format.fprintf fmt "?"
      else Format.fprintf fmt "(%a)?" pp_typ ty
    else
      Format.fprintf fmt "%a" pp_typ ty
end
