(** Properties of the source language that the translation to the functional
    core must respect.

    These are global mutable settings, read at each use rather than captured, so
    changing one affects every subsequent translation. The evaluation orders
    below are what [Optimize] relies on to decide, for each sub-expression,
    which assignments to a mutable variable may already have happened. Declaring
    an order the source language does not actually have makes the dataflow
    optimization unsound; {!UnknownOrder} is always a safe default. *)

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

(** Type given to expressions evaluated only for their effects (statements,
    loops, assignments). *)
let void_ty = ref Mlsem_types.Ty.unit

(** Order in which a function and its argument are evaluated. *)
let app_eval_order = ref LeftToRight

let tuple_eval_order = ref LeftToRight
let record_eval_order = ref LeftToRight
let cons_eval_order = ref LeftToRight

(** Evaluation order of the arguments of a user-defined constructor, by name.
    Constructors absent from the table default to {!UnknownOrder}. *)
let ccustom_eval_order : (string, eval_order) Hashtbl.t = Hashtbl.create 10
