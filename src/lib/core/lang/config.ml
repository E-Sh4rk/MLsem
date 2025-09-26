
type eval_order = LeftToRight | RightToLeft | UnknownOrder
let void_ty = ref Mlsem_types.Ty.unit
let app_eval_order = ref LeftToRight

(* TODO: specify eval order for constructors *)
