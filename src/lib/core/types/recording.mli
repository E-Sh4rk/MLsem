(** Capture of the tallying instances solved during type-checking, for
    benchmarking the constraint solver outside of MLsem. Recording is a global
    on/off switch and the log a global accumulator. *)

val start_recording : unit -> unit
val stop_recording : unit -> unit

val clear : unit -> unit
(** Empties the log. Recording never discards anything on its own, so this must
    be called between the runs one wants to keep apart. *)

type tally_call = Recording_internal.tally_call

val tally_calls : unit -> tally_call list
(** The instances recorded so far, in chronological order. *)

val save_to_file : string -> tally_call list -> unit
(** [save_to_file file calls] writes [calls] as JSON to [file] with its
    extension replaced by [.json]. Each instance is an object with fields
    ["vars"] / ["rvars"] (all the type / row variables of the constraints) and
    ["mono"] / ["rmono"] (those among them that are monomorphic, so ["mono"] is
    a subset of ["vars"]), plus ["constr"], a list of [[lhs, rhs]] pairs to be
    read as [lhs ≤ rhs]. Variables are renamed to short names, consistently
    within an instance but not across instances. *)
