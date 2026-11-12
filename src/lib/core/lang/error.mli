(** Typing errors of the language.

    They extend the errors of {!Mlsem_system.Error} by recognizing the failures
    that occur on the encodings introduced by {!MAst.to_system_ast}, so that
    they can be reported in terms of the original constructs. *)

open Mlsem_common
open Mlsem_types
open Mlsem_system.Annot

type kind =
| UnboundVar of Variable.t
| UntypeableApp of { fn: Ty.t ; arg: Ty.t }
| UntypeableConstructor of { expected: GTy.t list list ; given: GTy.t list }
| UntypeableRec of { funs: Variable.t list ; expected: Ty.t list ; given: Ty.t list }
| UntypeableEncoding
| UntypeableProjection of { arg: Ty.t ; domain: Ty.t }
| UntypeableCast of { check: Mlsem_system.Ast.check ; expected: GTy.t ; given: GTy.t }
| UntypeableCoercion of { check: Mlsem_system.Ast.check ; expected: GTy.t ; given: GTy.t }
| InvalidAnnot of string
(** The constructors above are those of {!Mlsem_system.Error.kind}. *)
| UntypeableAssignment of { var: Variable.t ; expected: Ty.t option ; given: Ty.t }
(** The value assigned to the mutable variable [var] has type [given],
    which is not a subtype of the type [expected] of the variable
    ([None] if the latter could not be determined). *)

type t = {
    eid: Eid.t ;  (** The identifier of the expression on which the error occurred *)
    expr: Mlsem_system.Ast.e ; (** The expression on which the error occurred *)
    kind: kind ;
    env: Env.t ;  (** The typing environment at that expression *)
    ann: IAnnot.a (** The (partial) annotation of that expression *)
  }

exception Untypeable of t

val of_system : Mlsem_system.Error.t -> t
(** Converts an error of {!Mlsem_system.Error}, recognizing the failures that
    occur on the encoding of an assignment. *)

(** {1 Default printers}

    Same as those of {!Mlsem_system.Error}. *)

type style = Mlsem_system.Error.style = Types | Witnesses | Both

val title : t -> string
val pp_descr : ?style:style -> Format.formatter -> t -> unit
val descr : ?style:style -> t -> string option
val pp : ?style:style -> Format.formatter -> t -> unit
