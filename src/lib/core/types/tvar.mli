open Base

(** @canonical Mlsem_types.Row *)
module Row = Sstt.Row

(** How inference may treat a type variable.

    - [KNoInfer]: rigid. Written by the user (as ['a]) and universally
      quantified on their behalf, so tallying must {b not} instantiate it — it
      is passed as monomorphic to every solving call, via
      [TVOp.all_vars KNoInfer].
    - [KInfer]: inferable. Either introduced by the reconstruction or written by
      the user as a weak variable (['_a]); tallying is free to instantiate it.
    - [KTemporary]: internal and short-lived, e.g. the placeholders used while
      resolving recursive type definitions, or the fresh names used for
      printing.

    @canonical Mlsem_types.kind *)
type kind = KNoInfer | KInfer | KTemporary

module type Var = sig
    type set
    type t

    val all_vars : kind -> set
    (** {b Every} variable of that kind created since the program started: this
        is a global registry, not a query about some term. It is how the set of
        rigid variables fed to tallying is obtained, and it grows monotonically
        for the lifetime of the process. *)

    val has_kind : kind -> t -> bool
    (** [false] for a variable that was not created by this module, where the
        accessors below raise instead. *)

    val kind : t -> kind
    (** @raise Not_found if the variable was not created by this module. *)

    val equal : t -> t -> bool
    val compare : t -> t -> int

    val name : t -> string
    (** @raise Invalid_argument if the variable was not created by this
        module. *)

    val prefix : t -> string
    (** The prefix under which the variable is printed (['] for type variables,
        [`] for row variables).
        @raise Invalid_argument if the variable was not created by this
        module. *)

    val mk : kind -> string option -> t
    (** Creates a {b fresh} variable, with the given display name if any.
        Variables are never deduplicated by name: calling this twice with the
        same name yields two distinct variables that print identically. A caller
        that wants a name to denote one variable must keep its own table. *)

    val pp : Format.formatter -> t -> unit
end
module type TVar = sig
    include Var
    val typ : t -> Ty.t
end
module type RVar = sig
    include Var
    val row : t -> Row.t
    val fty : t -> FTy.t
end

module type VarSet = sig
    type var

    include Set.S with type elt=var

    val union_many : t list -> t
    val inter_many : t list -> t
    val pp : Format.formatter -> t -> unit
end

(** @canonical Mlsem_types.TVar *)
module rec TVar : (TVar with type set := TVarSet.t and type t = Sstt.Var.t)

(** @canonical Mlsem_types.TVarSet *)
and TVarSet : (VarSet with type var := TVar.t and type t = Sstt.VarSet.t)

(** @canonical Mlsem_types.RVar *)
module rec RVar : (RVar with type set := RVarSet.t and type t = Sstt.RowVar.t)

(** @canonical Mlsem_types.RVarSet *)
and RVarSet : (VarSet with type var := RVar.t and type t = Sstt.RowVarSet.t)

(** @canonical Mlsem_types.MVarSet *)
module MVarSet = Sstt.MixVarSet

(** @canonical Mlsem_types.Subst *)
module Subst = Sstt.Subst

(** Decorrelation of row variables.

    A row variable stands for "all the fields other than those named
    explicitly", so the same row variable occurring in two record types that
    mention different labels does not stand for the same thing. Solving
    constraints over such types requires first replacing each row variable by a
    combination of fresh per-label variables — that is what a field context
    records, so that the solution can afterwards be expressed back in terms of
    the original variables.

    @canonical Mlsem_types.FieldCtx *)
module FieldCtx : sig
    type fvar = RVar.t * string
    (** An original row variable together with the label it was split on. *)

    type t

    val of_ty : RVarSet.t -> Ty.t -> t
    (** [of_ty mono ty] builds the context splitting the row variables of [ty]
        that are not in [mono], over the labels [ty] mentions. *)

    val of_tys : RVarSet.t -> Ty.t list -> t

    val decorrelate : t -> Ty.t -> Ty.t
    (** Rewrites a type in terms of the context's fresh per-label variables. *)

    val recombine : t -> Ty.t -> Ty.t
    (** Inverse of {!decorrelate}. *)

    val recombine' : t -> Subst.t -> Subst.t
    (** {!recombine} applied to a substitution obtained on decorrelated types,
        yielding a substitution over the original row variables. *)

    val fresh_vars : t -> RVarSet.t
    val fvar_of_fresh_var : t -> RVar.t -> fvar option
    (** Which original variable and label a fresh variable came from; [None] if
        it is not one of the context's fresh variables. *)

    val empty : t
    val merge : t -> t -> t
    val merge_many : t list -> t
end

(** @canonical Mlsem_types.TVOp *)
module TVOp : sig
    val all_vars : kind -> MVarSet.t
    (** Every type and row variable of that kind created since the program
        started (cf. [Var.all_vars]). [all_vars KNoInfer] is the set of rigid
        variables passed as monomorphic to {!tally}. *)

    val vars : Ty.t -> MVarSet.t
    (** All the variables of a type. This is a {b syntactic} notion inherited
        from sstt: a variable that does not affect the denotation of the type
        may or may not be reported, depending on the simplifications already
        applied to it. *)

    val vars' : Ty.t list -> MVarSet.t

    val top_vars : Ty.t -> MVarSet.t
    (** Only the variables occurring at top level, i.e. not under a type
        constructor. *)

    val strict_vars : Ty.t -> MVarSet.t
    (** Only the variables occurring strictly under a type constructor. *)

    val vars_of_kind : kind -> Ty.t -> MVarSet.t

    (** {3 Polarity}

        [`Pos] (resp. [`Neg]) means the type is monotonic (resp.
        anti-monotonic) in the variable, [`Both] that it is neither, and
        [`None] that the variable does not affect the denotation at all — which
        can happen because {!vars} is syntactic, so a reported variable may be
        vacuous. *)

    val polarity1 : TVar.t -> Ty.t -> [ `Both | `Neg | `Pos | `None ]
    val polarity2 : RVar.t -> Ty.t -> [ `Both | `Neg | `Pos | `None ]
    val polarity1' : TVar.t -> Ty.t list -> [ `Both | `Neg | `Pos | `None ]
    val polarity2' : RVar.t -> Ty.t list -> [ `Both | `Neg | `Pos | `None ]

    (** The primed variants below combine the polarities over a list of types,
        as if the variable occurred in each of them. Variables of polarity
        [`None] are omitted from the results. *)

    val vars_with_polarity1 : Ty.t -> (TVar.t * [ `Both | `Neg | `Pos ]) list
    val vars_with_polarity2 : Ty.t -> (RVar.t * [ `Both | `Neg | `Pos ]) list
    val vars_with_polarity1' : Ty.t list -> (TVar.t * [ `Both | `Neg | `Pos ]) list
    val vars_with_polarity2' : Ty.t list -> (RVar.t * [ `Both | `Neg | `Pos ]) list

    val is_ground_typ : Ty.t -> bool

    val refresh : ?preserve_names:bool -> kind:kind -> MVarSet.t -> Subst.t
    (** A substitution renaming each given variable to a fresh one of the given
        kind. With [~preserve_names:true] the fresh variables keep the display
        name of the originals, which makes them indistinguishable when printed
        with {!TVar.pp}. *)

    val shorten_names : ?kind:kind -> MVarSet.t -> Subst.t
    (** A substitution renaming the given variables to fresh ones named [a],
        [b], … — for display only. *)

    val pp_typ_short : Format.formatter -> Ty.t -> unit
    (** Prints with variables renamed by {!shorten_names}. *)

    val pp_typ_uniq : Format.formatter -> Ty.t -> unit
    (** Prints with variables renamed so that distinct variables print
        differently, even when they share a display name. *)

    val pp_typ_subst : Subst.t -> Format.formatter -> Ty.t -> unit
    (** Prints the type after applying the given substitution. *)

    (** [clean p n mono t] substitutes in [t]
        all variables not in [mono] and only occurring positively by [p], and
        all variables not in [mono] and only occurring negatively by [n] *)
    val clean : pos1:Ty.t -> neg1:Ty.t -> pos2:Row.t -> neg2:Row.t -> MVarSet.t -> Ty.t -> Ty.t
    val clean_subst : pos1:Ty.t -> neg1:Ty.t -> pos2:Row.t -> neg2:Row.t -> MVarSet.t -> Ty.t -> Subst.t
    val clean' : pos1:Ty.t -> neg1:Ty.t -> pos2:Row.t -> neg2:Row.t -> MVarSet.t -> Ty.t list -> Ty.t list
    val clean_subst' : pos1:Ty.t -> neg1:Ty.t -> pos2:Row.t -> neg2:Row.t -> MVarSet.t -> Ty.t list -> Subst.t

    val bot_instance : MVarSet.t -> Ty.t -> Ty.t
    (** The smallest instance of the type, obtained by {!clean}ing the variables
        outside [mono] with [empty] positively and [any] negatively. *)

    val top_instance : MVarSet.t -> Ty.t -> Ty.t
    (** Dual of {!bot_instance}: the largest instance. *)

    val tally : ?record:bool -> MVarSet.t -> (Ty.t * Ty.t) list -> Subst.t list
    (** [tally mono cs] returns substitutions — of the variables {e not} in
        [mono] — that make every constraint [(s,t)] of [cs] satisfy [s ≤ t].
        The empty list means the constraints are unsatisfiable. Set
        [~record:false] to keep the instance out of {!Recording}'s log; this
        matters for calls made while comparing candidate solutions, which are
        not part of the derivation being built. *)

    val tally_const_rows : ?record:bool -> MVarSet.t -> (Ty.t * Ty.t) list -> Subst.t list
    (** Like {!tally}, but row variables are not instantiated with rows that
        introduce new fields. *)

    val decompose : MVarSet.t -> Subst.t -> Subst.t -> Subst.t list
    (** [decompose mono s1 s2] returns the substitutions [s] such that applying
        [s] after [s2] is at least as precise as [s1]; the empty list when [s1]
        is not an instance of [s2]. Used to compare candidate solutions. *)

    val factorize : TVarSet.t * TVarSet.t -> Ty.t -> Ty.t * Ty.t
    (** [factorize (pvs,nvs) t] splits [t] into a part that can be factorized by
        [pvs] and [nvs], and a remainder: it returns [(t1,t2)] such that
        [t] is equivalent to [(/\pvs /\ ~\/nvs /\ t1) \/ t2].
        The factorized part gathers the DNF lines of [t] in which every variable
        of [pvs] occurs positively and every variable of [nvs] occurs
        negatively, with those occurrences removed. *)
end
