
type eval_order = LeftToRight | RightToLeft | UnknownOrder
let void_ty = ref Mlsem_types.Ty.unit
let app_eval_order = ref LeftToRight
let tuple_eval_order = ref LeftToRight
let record_eval_order = ref LeftToRight
let recupd_eval_order = ref LeftToRight
let cons_eval_order = ref LeftToRight
let ccustom_eval_order : (string, eval_order) Hashtbl.t = Hashtbl.create 10
