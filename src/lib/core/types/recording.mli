
val start_recording : unit -> unit
val stop_recording : unit -> unit

val record : Sstt.VarSet.t -> Sstt.Var.t list -> (Sstt.Ty.t * Sstt.Ty.t) list -> unit
val clear : unit -> unit

type tally_call = {
    vars : Sstt.Var.t list ;
    mono : Sstt.VarSet.t ;
    priority : Sstt.Var.t list ;
    constraints : (Sstt.Ty.t * Sstt.Ty.t) list
    }
val tally_calls : unit -> tally_call list
