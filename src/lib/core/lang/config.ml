
type eval_order =
| LeftToRight
(** Evaluates arguments left-to-right. *)

| RightToLeft
(** Evaluates arguments right-to-left. *)

| UnknownOrder
(** Default. Evaluation order is unknown (and/or optional).
    Once started, evaluation cannot be aborted (e.g. Ternary, Join, Meet). *)

| Abortable
(** Evaluation order is unknown (and/or optional).
    Evaluation can be aborted (e.g. Voidify, Try). *)

let void_ty = ref Mlsem_types.Ty.unit
let app_eval_order = ref LeftToRight
let tuple_eval_order = ref LeftToRight
let record_eval_order = ref LeftToRight
let cons_eval_order = ref LeftToRight
let ccustom_eval_order : (string, eval_order) Hashtbl.t = Hashtbl.create 10
