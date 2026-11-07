open Mlsem_common
open Mlsem_types
open Annot
open Mlsem_utils

type kind =
| UnboundVar of Variable.t
| UntypeableApp of { fn: Ty.t ; arg: Ty.t }
| UntypeableConstructor of { expected: GTy.t list list ; given: GTy.t list }
| UntypeableRec of { funs: Variable.t list ; expected: Ty.t list ; given: Ty.t list }
| UntypeableEncoding
| UntypeableProjection of { arg: Ty.t }
| UntypeableCast of { check: Ast.check ; expected: GTy.t ; given: GTy.t }
| UntypeableCoercion of { check: Ast.check ; expected: GTy.t ; given: GTy.t }
| InvalidAnnot of string

type t = {
    eid: Eid.t ;
    kind: kind ;
    env: Env.t ;
    ann: IAnnot.a
  }

exception Untypeable of t

let title t =
  match t.kind with
  | UnboundVar _ -> "unbound variable"
  | UntypeableApp _ -> "untypeable application"
  | UntypeableConstructor _ -> "untypeable constructor"
  | UntypeableRec _ -> "untypeable recursive function"
  | UntypeableEncoding -> "untypeable encoding"
  | UntypeableProjection _ -> "untypeable projection"
  | UntypeableCast _ -> "untypeable cast"
  | UntypeableCoercion _ -> "untypeable coercion"
  | InvalidAnnot msg -> msg

let pp_expected_given check fmt (expected, given) =
  match check with
  | Ast.Check ->
    Format.fprintf fmt "expected: @[<h>%a@]@.given: @[<h>%a@]"
      GTy.pp expected GTy.pp given
  | Ast.CheckStatic ->
    Format.fprintf fmt "expected: @[<h>%a@]@.given: @[<h>%a@]"
      Ty.pp (GTy.lb expected) Ty.pp (GTy.lb given)
  | Ast.NoCheck -> ()

let pp_descr fmt t =
  match t.kind with
  | UnboundVar v -> Format.fprintf fmt "name: %a" Variable.pp v
  | UntypeableApp { fn ; arg } ->
    Format.fprintf fmt "function: @[<h>%a@]@.argument: @[<h>%a@]" Ty.pp fn Ty.pp arg
  | UntypeableConstructor { expected ; given } ->
    Format.fprintf fmt "expected: @[<h>%a@]@.given: @[<h>%a@]"
      (Utils.pp_seq (Utils.pp_seq GTy.pp " ; ") " ;; ") expected
      (Utils.pp_seq GTy.pp " ; ") given
  | UntypeableRec _ -> Format.fprintf fmt "cannot unify the body with self"
  | UntypeableEncoding -> ()
  | UntypeableProjection { arg } -> Format.fprintf fmt "argument: @[<h>%a@]" Ty.pp arg
  | UntypeableCast { check ; expected ; given }
  | UntypeableCoercion { check ; expected ; given } ->
    pp_expected_given check fmt (expected, given)
  | InvalidAnnot _ -> ()

let descr t =
  match Format.asprintf "%a" pp_descr t with
  | "" -> None
  | d -> Some d

let pp fmt t =
  match descr t with
  | None -> Format.fprintf fmt "%s" (title t)
  | Some d -> Format.fprintf fmt "%s:@.%s" (title t) d
