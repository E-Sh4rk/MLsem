(** Identifiers for expressions.

    Every node of an AST carries one, and several analyses are indexed by it
    ([Mlsem_system.Refinement.Refinements], [Mlsem_system.Analyzer]), so
    distinct nodes of a same term must carry distinct ids: a program
    transformation that duplicates a sub-expression must refresh the ids of the
    copies. Each id also records a source location and whether the analyzer may
    report notices about it, so that generated code is not blamed for problems
    the user cannot see. *)

type t

val dummy : t
(** Placeholder id, for expressions that do not correspond to any source
    location. It has no location and no notices, and [refresh dummy] is
    [dummy]. *)

val unique : unit -> t
(** Fresh id for generated code: no source location, and notices disabled.
    Only appropriate on nodes that cannot be blamed for a type error. *)

val unique_with_pos : Position.t -> t
(** Fresh id located at [pos], with notices {e enabled}. Combine with
    [set_show_notices] to locate a generated node without making it eligible
    for the analyzer's reports. *)

val refresh : t -> t
(** Fresh id inheriting the location and the notice flag of its argument. This
    is how a duplicated sub-expression keeps its diagnostics while getting the
    distinct ids the analyses require. *)

val loc : t -> Position.t
(** @raise Not_found if the id was not produced by this module. *)

val set_show_notices : t -> bool -> unit
(** @raise Not_found if the id was not produced by this module. *)

val show_notices : t -> bool
(** Whether the analyzer may report notices and unreachability warnings about
    this expression.
    @raise Not_found if the id was not produced by this module. *)

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit    
