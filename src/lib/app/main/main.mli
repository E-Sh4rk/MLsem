open Mlsem_common
open Mlsem_types
module MVariable = Mlsem_lang.MVariable

module NameMap : Map.S with type key=string
type message = Mlsem_system.Analyzer.severity * Position.t * string * string option
type inferred = {
  var: Variable.t;
  ty: TyScheme.t;
  declared: bool;
}
type treat_result =
| TSuccess of inferred list * message list * float
| TDone
| TFailure of Variable.t option * Position.t * string * string option * float

type envs = Builder.benv * Variable.t NameMap.t * Ty.t list VarMap.t * Env.t * PEnv.t
val treat : envs -> 'a Position.located * PAst.element -> envs * treat_result
val treat_sig : envs -> 'a Position.located * PAst.element -> envs * treat_result
val treat_def : envs -> 'a Position.located * PAst.element -> envs * treat_result
val treat_all_sigs : envs -> ('a Position.located * PAst.element) list -> envs * treat_result

val initial_envs : envs
val initial_senv : Ty.t list VarMap.t
val initial_benv : Builder.benv
val initial_penv : PEnv.t

(* Render an [inferred.ty] using the type-alias environment carried in [envs].
   [display] gives the short form shown to users; [signature] gives the
   unquantified form suitable for a [val] declaration. *)
val display : envs -> TyScheme.t -> string
val signature : envs -> TyScheme.t -> string

type parsing_result =
| PSuccess of PAst.program
| PFailure of Position.t * string

val parse : [< `File of string | `String of string ] -> parsing_result
