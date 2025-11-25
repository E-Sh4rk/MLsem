
val start_recording : unit -> unit
val stop_recording : unit -> unit

val clear : unit -> unit

type tally_call = Recording_internal.tally_call
val tally_calls : unit -> tally_call list

val save_to_file : string -> tally_call list -> unit
