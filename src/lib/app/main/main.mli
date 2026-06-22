open Mlsem_common
open Mlsem_types
module MVariable = Mlsem_lang.MVariable

module NameMap : Map.S with type key=string
type message = Mlsem_system.Analyzer.severity * Position.t * string * string option
type inferred = {
  var: Variable.t;
  ty: TyScheme.t;
  sigs: Signature.t;
  declared: bool;
}
type result =
| TSuccess of inferred list * float
| TDone
| TFailure of Variable.t option * Position.t * string * string option * float
type output = { res:result ; msg:message list }

type envs = Builder.benv * Variable.t NameMap.t * Signature.t VarMap.t * Env.t * PEnv.t
val treat : envs -> 'a Position.located * PAst.element -> envs * output
val treat_sig : envs -> 'a Position.located * PAst.element -> envs * output
val treat_def : envs -> 'a Position.located * PAst.element -> envs * output
val treat_all_sigs : envs -> ('a Position.located * PAst.element) list -> envs * output

val initial_envs : envs
val initial_senv : Signature.t VarMap.t
val initial_benv : Builder.benv
val initial_penv : PEnv.t

val display : envs -> TyScheme.t -> string
val signature : envs -> Signature.t -> string list

(* Build the concrete type denoted by a surface type expression, resolved
   against the given environment. Raises on a parse/elaboration failure. *)
val build_type : envs -> string -> Ty.t

(* User-defined type names known in the environment, for concrete-type
   suggestions in the type toolkit. *)
val user_type_names : envs -> string list

type parsing_result =
| PSuccess of PAst.program
| PFailure of Position.t * string

val parse : [< `File of string | `String of string ] -> parsing_result
