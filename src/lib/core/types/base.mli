
(** @canonical Mlsem_types.PrinterCfg *)
module PrinterCfg : sig
    open Sstt.Printer
    open Sstt.Extensions
    val set_bool_printer : (int -> Sstt.Prec.assoc -> Format.formatter -> Bools.t -> unit) -> unit
    val set_float_printer : (int -> Sstt.Prec.assoc -> Format.formatter -> Floats.t -> unit) -> unit
    val set_string_printer : (int -> Sstt.Prec.assoc -> Format.formatter -> Strings.t -> unit) -> unit
    val set_list_printer : (int -> Sstt.Prec.assoc -> Format.formatter -> Lists.t -> unit) -> unit
    val set_char_printer : (int -> Sstt.Prec.assoc -> Format.formatter -> Chars.t -> unit) -> unit
    val set_abstract_printer :
        (Sstt.Tag.t -> int -> Sstt.Prec.assoc -> Format.formatter -> descr Abstracts.t -> unit) -> unit
    val set_unit_printer : string -> unit
    val set_dyn_printer : string -> unit
    val set_tvar_prefix : string -> unit
    val set_rvar_prefix : string -> unit
    val set_descr_printer : (int -> Sstt.Prec.assoc -> Format.formatter -> descr -> unit) -> unit
    val set_printer : (Format.formatter -> descr t -> unit) -> unit

    val add_abstract_type : Sstt.Tag.t -> unit
    val add_printer_param : params -> unit

    val printer_params : unit -> params
    val print_descr_ctx : int -> Sstt.Prec.assoc -> Format.formatter -> descr -> unit
    val print_descr : Format.formatter -> descr -> unit
    val print : Format.formatter -> descr t -> unit
    val print_dyn : Format.formatter -> unit -> unit
    val tvar_prefix : unit -> string
    val rvar_prefix : unit -> string
end

(** Printing environment: the type aliases to fold back when printing a type,
    so that a type built from [type list('a) = …] prints as [list(int)] rather
    than as its expansion.

    The environment is carried as an {b effect} rather than threaded explicitly:
    [register] performs an [Update], and printing performs a [Get]. Both are
    therefore only usable inside a [sequential_handler], and raise
    [Effect.Unhandled] outside one.

    @canonical Mlsem_types.PEnv *)
module PEnv : sig
    type t (* Printing environment *)
    type _ Effect.t += Update: t -> unit Effect.t
    type _ Effect.t += Get: t Effect.t

    val sequential_handler : t -> ('a -> 'b) -> 'a -> 'b * t
    (** [sequential_handler penv f x] runs [f x] with [penv] as the ambient
        printing environment, and returns its result together with the
        environment as the registrations performed by [f] left it. *)

    val empty : t

    val merge : t -> t -> t
    (** Adds the aliases of the second environment to the first, dropping any
        earlier alias that has the same name or denotes an equivalent type. *)

    val merge' : t list -> t

    (* Alias registering (for pretty printing) *)

    val register : string -> Sstt.Ty.t -> unit
    (** Registers a name for a type. Must be called under a
        [sequential_handler]. *)

    val register_parametrized : string -> Sstt.Ty.t list -> Sstt.Ty.t -> unit
    (** [register_parametrized name args ty] registers [ty] under the display
        form [name(args)]. Type variables occurring in [args] are recorded as
        holes, so that the alias still prints correctly when those variables are
        renamed at printing time. Must be called under a
        [sequential_handler]. *)

    (* Pretty-printing *)

    val printer_params : unit -> Sstt.Printer.params
    (** Printer parameters for the ambient environment. Must be called under a
        [sequential_handler]. *)

    val printer_params' : Sstt.Subst.t -> Sstt.Printer.params
    (** Like {!printer_params}, for printing types to which the given
        substitution is applied — the aliases are substituted accordingly. *)
end

(** @canonical Mlsem_types.Ty *)
module Ty : sig
    type t = Sstt.Ty.t

    val pp : Format.formatter -> t -> unit
    val pp' : Sstt.Subst.t -> Format.formatter -> t -> unit
    val pp_raw : Format.formatter -> t -> unit

    val any : t
    val empty : t

    val tt : t
    val ff : t
    val bool : t
    val int : t
    val float : t
    val char : t
    val unit : t
    val string : t

    val interval : Z.t option -> Z.t option -> t
    val char_interval : char -> char -> t
    val string_lit : string -> t

    val neg : t -> t
    val cup : t -> t -> t
    val cap : t -> t -> t
    val diff : t -> t -> t
    val conj : t list -> t
    val disj : t list -> t

    val is_empty : t -> bool
    val is_any : t -> bool
    val non_empty: t -> bool
    val non_any : t -> bool
    val leq  : t -> t -> bool
    val disjoint : t -> t -> bool
    val equiv : t -> t -> bool

    val factorize : t -> t
    (** Rewrites the type by putting in common the parts shared by the lines of
        its disjunctive normal form. Preserves the denotation. *)

    val simplify : t -> t
    (** Searches for a smaller representation of the type by removing redundant
        parts of its normal form. Preserves the denotation, and is more thorough
        — and more expensive — than {!factorize}. *)
end

(** @canonical Mlsem_types.FTy *)
module FTy : sig
    type t = Sstt.Ty.F.t

    val any : t
    val empty : t

    val of_oty : Ty.t * bool -> t

    val neg : t -> t
    val cup : t -> t -> t
    val cap : t -> t -> t
    val diff : t -> t -> t
    val conj : t list -> t
    val disj : t list -> t
end

(** @canonical Mlsem_types.Enum *)
module Enum : sig
    type t
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
    val define : string -> t
    val any : Ty.t
    val typ : t -> Ty.t
end

(** @canonical Mlsem_types.Tag *)
module Tag : sig
    type t
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
    val define : string -> t
    (** Creates a {b fresh} tag; two calls with the same name yield distinct
        tags that print identically. *)

    val any : Ty.t
    val mk : t -> Ty.t -> Ty.t
    val proj : t -> Ty.t -> Ty.t
    (** The type carried by the tag. Only meaningful on a type all of whose
        values carry that tag, i.e. below [mk tag Ty.any]. *)

    val tag : t -> Sstt.Tag.t
end

(** Abstract types: opaque parameterized type constructors, such as [ref('a)],
    whose parameters are {b invariant}.

    @canonical Mlsem_types.Abstract *)
module Abstract : sig
    type t

    val define : string -> int -> t
    (** [define name arity] creates a {b fresh} abstract type constructor. *)

    val arity : t -> int
    val any : t -> Ty.t
    val mk : t -> Ty.t list -> Ty.t

    val dnf : t -> Ty.t -> (Ty.t list) list list
    (** The disjunctive normal form of the given abstract type's part of a type:
        a disjunction of conjunctions of parameter tuples. *)

    val top_transform :
        (t * (Ty.t list list * Ty.t list list) list
          -> (Ty.t list list * Ty.t list list) list)
        -> Ty.t -> Ty.t
    (** Rewrites the abstract atoms occurring at the {b top level} of a type.
        The function receives, for one abstract constructor, its normal form as
        a list of (positive tuples, negative tuples) pairs. *)

    val transform :
        (t * (Ty.t list list * Ty.t list list) list
          -> (Ty.t list list * Ty.t list list) list)
        -> Ty.t -> Ty.t
    (** Like {!top_transform}, but rewrites abstract atoms at {b every} depth,
        including under other constructors and inside recursive types. *)
end

(** @canonical Mlsem_types.Tuple *)
module Tuple : sig
    val any : Ty.t
    val any_n : int -> Ty.t
    val mk : Ty.t list -> Ty.t
    val proj : int -> int -> Ty.t -> Ty.t
    val dnf : int -> Ty.t -> Ty.t list list
    val of_dnf : int -> Ty.t list list -> Ty.t
    val decompose : Ty.t -> (int * Ty.t list list) list * bool
    val recompose : (int * Ty.t list list) list * bool -> Ty.t
end

(** @canonical Mlsem_types.Lst *)
module Lst : sig
    val nil : Ty.t
    val any : Ty.t
    val any_non_empty : Ty.t
    val cons : Ty.t -> Ty.t -> Ty.t
    val dnf : Ty.t -> (Ty.t * Ty.t) list
    val proj : Ty.t -> Ty.t * Ty.t
end

(** @canonical Mlsem_types.Record *)
module Record : sig
    type oty = Ty.t*bool
    (** A field type together with a flag saying whether the field may be
        absent. *)

    val mk : oty (* tail *) -> (string * oty) list -> Ty.t
    (** [mk tail fields] is the record type with the given fields; [tail] is the
        type of every field {e not} listed. *)

    val mk_open : (string * oty) list -> Ty.t
    (** {!mk} with an unconstrained tail: other fields may be present, with any
        type. *)

    val mk_closed : (string * oty) list -> Ty.t
    (** {!mk} with an empty tail: no other field may be present. *)

    val mk' : FTy.t -> (string * FTy.t) list -> Ty.t
    (** Like {!mk}, taking field types that may involve row variables. *)

    val any : Ty.t
    val any_with : string -> Ty.t
    val any_without : string -> Ty.t

    val dnf : Ty.t -> ((string * oty) list * oty) list
    (** The record part of a type, as a disjunction of [(fields, tail)]. *)

    val dnf' : Ty.t -> ((string * FTy.t) list * FTy.t) list
    (** Like {!dnf}, preserving row variables in the field types. *)

    val of_dnf : ((string * oty) list * oty) list -> Ty.t
    val of_dnf' : ((string * FTy.t) list * FTy.t) list -> Ty.t

    val proj : Ty.t -> string -> Ty.t
    (** The type of a field. Only meaningful on a type whose values all have
        that field. *)

    val merge : Ty.t -> Ty.t -> Ty.t
    (** Record concatenation: the fields of the second override those of the
        first. Returns [Ty.empty] if either argument has no record part. *)

    val remove_field : Ty.t -> string -> Ty.t
    (** The type with the given field removed. Returns [Ty.empty] if the
        argument has no record part. *)

    val from_label : Sstt.Label.t -> string
    (** The name a label was created from. Labels that entered the type
        representation through another route have no name here, and are given a
        synthetic one of the form [__reservedN] — so a name obtained from
        {!dnf} is not necessarily a name the user ever wrote. *)

    val to_label : string -> Sstt.Label.t
    (** The label of a field name; the same name always yields the same
        label. *)
end

(** @canonical Mlsem_types.Arrow *)
module Arrow : sig
    val mk : Ty.t -> Ty.t -> Ty.t
    val any : Ty.t
    val domain : Ty.t -> Ty.t
    val apply : Ty.t -> Ty.t -> Ty.t
    val dnf : Ty.t -> (Ty.t * Ty.t) list list
    val of_dnf : (Ty.t * Ty.t) list list -> Ty.t
end
