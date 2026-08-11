(** The functional core language.

    The full language of [Mlsem_lang.Ast] is compiled down to this small
    lambda-calculus, which is what {!Checker} and {!Reconstruction} work on.

    Terms are expected to be alpha-converted (every binder introduces a distinct
    {!Mlsem_common.Variable.t}) and to carry pairwise distinct [Eid.t]s. *)

open Mlsem_common
open Mlsem_types

type pcustom = { pname: string ; pdom: Ty.t -> Ty.t ; proj: Ty.t -> Ty.t ; pgen: bool }
(** A user-defined projection. [proj] computes the type of the result from the
    type of the argument and must be monotonic; [pdom t] is the largest argument
    type whose projection is below [t]; [pgen] says whether the projection is
    generalizable, i.e. free of effects (cf. {!Checker.generalize}). *)

type ccustom = { cname: string ; cdom: Ty.t -> Ty.t list list ; cons: Ty.t list -> Ty.t ; cgen: bool }
(** A user-defined constructor. [cons] computes the type of the result from the
    types of the arguments and must be monotonic; [cdom t] returns the
    alternative argument-type tuples whose construction is below [t], each of
    which must have the constructor's arity; [cgen] as in {!pcustom}. *)

type ocustom = { oname: string ; ofun: Env.t -> TyScheme.t ; ogen: bool }
(** A user-defined operation, typed by the arrow scheme [ofun] returns.
    [ogen] as in {!pcustom}. *)

type check = Check | CheckStatic | NoCheck
(** How much of a cast or coercion is verified statically: [Check] requires both
    bounds of the gradual type to be respected, [CheckStatic] only the lower one
    (the rest being deferred to a runtime check), and [NoCheck] nothing. *)

type projection = (* Projections must be monotonic operations *)
| Pi of int * int | PiField of string | PiFieldOpt of string
| Hd | Tl | PiTag of Tag.t | PCustom of pcustom

type constructor = (* Constructors must be monotonic operations *)
| Tuple of int | Cons | Rec of string list * bool | Tag of Tag.t | Enum of Enum.t 
| Join of int | Meet of int | Ternary of Ty.t (* Should not contain type vars *)
| Voidify of Ty.t (* Should not contain type vars *)
| Normalize | CCustom of ccustom

type operation =
| RecUpd of string | RecDel of string | Ignore of Ty.t
| OCustom of ocustom

type alt_settings = { aname: string ; amask: Env.t -> bool list ; aerror: Env.t -> string }
(** Settings of an [Alt] expression. [amask env] selects the branches that may be
    typed under [env]: it must return exactly one boolean per branch of the
    [Alt], in the same order.
    [aerror env] builds the error message reported when no branch could be typed. *)

type param_annot = GTy.t option

type e =
| Value of GTy.t
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of param_annot * Variable.t * t
| LambdaRec of (param_annot * Variable.t * t) list
| Ite of t * GTy.t * t * t
| App of t * t
| Operation of operation * t
| Projection of projection * t
| Let of (Ty.t list) (* empty list = no partitioning *) * Variable.t * t * t
(** The type list is a suggested decomposition of the bound variable: the body
    is typed once per cell and the results are joined, which is what lets a
    single definition be used at several types in a same scope. An empty list
    disables partitioning; note that only a partitioned [Let] propagates the
    divergence of its definition (an empty definition type makes every cell
    vanish). *)
| TypeCast of t * GTy.t * check
| TypeCoerce of t * GTy.t * check
| Alt of alt_settings * t list
and t = Eid.t * e

val map : (t -> t) -> t -> t
(** Bottom-up: [f] is applied to a node {e after} its children have been
    rewritten, and is not re-applied to the node it returns. *)

val map' : (t -> t option) -> t -> t
(** Top-down with early stop: [f] is applied to a node first, and when it
    returns [Some e'] the node is replaced by [e'] {e without} descending into
    it. Returning [None] recurses into the children. *)

val iter : (t -> unit) -> t -> unit
(** Bottom-up traversal, as {!map}. *)

val iter' : (t -> bool (* continue inside *)) -> t -> unit
(** Top-down traversal, as {!map'}: descends into a node only if [f] returns
    [true] on it. *)

val fv : t -> VarSet.t
(** Free variables, computed as "used minus bound" over the whole term. This is
    only correct because terms are alpha-converted, so a variable bound in one
    sub-term cannot occur free in another. *)

val vars : t -> VarSet.t
(** All variables, used or bound. *)

val apply_subst : Subst.t -> t -> t
(** Substitutes type variables in the type annotations of the term. The types
    embedded in [Ternary], [Voidify], [Ignore] and in the [pcustom] / [ccustom]
    / [ocustom] closures are {b not} traversed: they are required to be free of
    type variables. *)

val refresh : t -> t
(** Refreshes all Eid.t, preserving localization data.
    Useful when duplicating an expression. *)

val pp_raw : Format.formatter -> t -> unit
(** Prints the term as a data structure, annotations included; {!pp} prints it
    as source-like syntax. *)
val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val pp_check : Format.formatter -> check -> unit
val pp_projection : Format.formatter -> projection -> unit
val pp_constructor : Format.formatter -> constructor -> unit
val pp_operation : Format.formatter -> operation -> unit
val pp_alt_settings : Format.formatter -> alt_settings -> unit
val pp_param_annot : Format.formatter -> param_annot -> unit
val pp_pcustom : Format.formatter -> pcustom -> unit
val pp_ccustom : Format.formatter -> ccustom -> unit


(** {2 Typing of the primitives} *)

val domain_of_proj : projection -> Ty.t -> Ty.t
(** [domain_of_proj p t] is the largest argument type whose projection by [p] is
    below [t]. In particular [domain_of_proj p Ty.any] is the domain of [p]. *)

val proj : projection -> Ty.t -> Ty.t
(** The type of the projection of an argument of the given type. Monotonic;
    only meaningful on a type within the projection's domain. *)

val domains_of_construct : constructor -> Ty.t -> Ty.t list list
(** [domains_of_construct c t] returns the alternative argument-type tuples
    whose construction by [c] is below [t]; the empty list means [c] cannot
    produce a value in [t]. Each tuple has the arity of [c]. *)

val construct : constructor -> Ty.t list -> Ty.t
(** The type of the construction of arguments of the given types. Monotonic.
    @raise Invalid_argument if the number of arguments does not match [c]. *)

val fun_of_operation : Env.t -> operation -> TyScheme.t
(** The arrow scheme an operation is typed with; an operation is applied like a
    function whose type is fixed rather than inferred. *)

(** {2 Coercions} *)

val coerce : ?coercion_id:Eid.t -> check -> GTy.t -> t -> t
(** [coerce c ty e] wraps [e] in a [TypeCoerce] to [ty], {e pushing} the coercion
    inwards as far as the shape of [e] allows: coercing a lambda to an arrow
    type coerces its body and annotates its parameter, coercing a constructor
    coerces its arguments, and so on. This is what lets a user signature guide
    the reconstruction inside a definition instead of only constraining its
    result. The outer coercion is always kept as well, under [coercion_id] if
    given and under a refreshed id otherwise; sub-terms whose shape does not
    match the target type are simply left alone. *)

val push_coercions : t -> t
(** Applies {!coerce} to every [TypeCoerce] already present in the term. *)

val push_coercions' : t -> t
(** Applies {!coerce} to every [TypeCoerce] already present in the term,
    and duplicates [Lambda] expressions that are coerced into an intersection
    of arrows, so that it is possible to push the coercion inside. *)
