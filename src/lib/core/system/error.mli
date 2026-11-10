(** Typing errors, as reported by {!Checker} and {!Reconstruction}.

    An error only records the data that explains the failure; its textual
    rendering is left to the client, {!pp} being a default printer. *)

open Mlsem_common
open Mlsem_types
open Annot

type kind =
| UnboundVar of Variable.t
| UntypeableApp of { fn: Ty.t ; arg: Ty.t }
(** No instance of the function type accepts the argument type. *)
| UntypeableConstructor of { expected: GTy.t list list ; given: GTy.t list }
(** The arguments do not fit any of the domains of the constructor
    (each domain being a list with one type per argument). *)
| UntypeableRec of { funs: Variable.t list ; expected: Ty.t list ; given: Ty.t list }
(** The bodies of the mutually recursive functions [funs] cannot be unified
    with their own types: the body types [given] must be subtypes of the
    types [expected] annotated for the functions (one per function, in order). *)
| UntypeableEncoding
(** None of the alternatives of an encoding is typeable. *)
| UntypeableProjection of { arg: Ty.t ; domain: Ty.t }
| UntypeableCast of { check: Ast.check ; expected: GTy.t ; given: GTy.t }
| UntypeableCoercion of { check: Ast.check ; expected: GTy.t ; given: GTy.t }
| InvalidAnnot of string
(** The annotation given to {!Checker} is not a valid derivation.
    In normal operation the reconstruction only produces valid annotations, so
    this error indicates an internal inconsistency rather than a user type
    error — user errors are reported by the reconstruction. *)

type t = {
    eid: Eid.t ;  (** The identifier of the expression on which the error occurred *)
    expr: Ast.e ; (** The expression on which the error occurred *)
    kind: kind ;
    env: Env.t ;  (** The typing environment at that expression *)
    ann: IAnnot.a (** The (partial) annotation of that expression *)
  }

exception Untypeable of t
(** Raised by {!Checker} and {!Reconstruction} when an expression is untypeable. *)

(** {1 Default printers} *)

val title : t -> string
(** A short, one-line summary of the error (e.g. ["untypeable application"]). *)

type style =
| Types
(** Print the types involved (e.g. the function and argument types). *)
| Witnesses
(** Print a witness of the failure: a value that is in the given type but not
    in the expected one (e.g. an argument outside the domain of the function).
    Falls back to [Types] when no such value exists (e.g. when the failure
    comes from polymorphism rather than from a plain subtyping mismatch). *)
| Both
(** Print the types, followed by a witness if there is one. *)

val pp_descr : ?style:style -> Format.formatter -> t -> unit
(** Prints the details of the error, according to [style] (default [Types]).
    Prints nothing if there are no details. *)

val descr : ?style:style -> t -> string option
(** The output of {!pp_descr}, or [None] if it is empty. *)

val pp : ?style:style -> Format.formatter -> t -> unit
(** Prints the {!title} followed by the {!descr}, if any. *)
