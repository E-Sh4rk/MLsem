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
| UntypeableProjection of { arg: Ty.t ; domain: Ty.t }
| UntypeableCast of { check: Ast.check ; expected: GTy.t ; given: GTy.t }
| UntypeableCoercion of { check: Ast.check ; expected: GTy.t ; given: GTy.t }
| InvalidAnnot of string

type t = {
    eid: Eid.t ;
    expr: Ast.e ;
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
  | UntypeableProjection { arg ; _ } -> Format.fprintf fmt "argument: @[<h>%a@]" Ty.pp arg
  | UntypeableCast { check ; expected ; given }
  | UntypeableCoercion { check ; expected ; given } ->
    pp_expected_given check fmt (expected, given)
  | InvalidAnnot _ -> ()

(* Witness-based descriptions: instead of printing the types involved,
   print a value that is in the given type but not in the expected one. *)

let witness_of_diff given expected =
  Ty.diff given expected |> Witness.of_ty

(* First non-empty difference among the [(given, expected)] pairs *)
let witness_of_diffs lst =
  List.find_map (fun (g, e) -> witness_of_diff g e) lst

let witness_descr t =
  let pp_w what fmt w =
    Format.fprintf fmt "%s may be: @[<h>%a@]" what Witness.pp w in
  let gty_pairs check expected given =
    match check with
    | Ast.Check ->
      [ GTy.lb given, GTy.lb expected ; GTy.ub given, GTy.ub expected ]
    | Ast.CheckStatic -> [ GTy.lb given, GTy.lb expected ]
    | Ast.NoCheck -> []
  in
  let res =
    match t.kind with
    | UnboundVar _ | UntypeableEncoding | InvalidAnnot _ -> None
    | UntypeableApp { fn ; arg } ->
      begin match witness_of_diff fn Arrow.any with
      | Some w -> Some (fun fmt -> pp_w "function (not a function)" fmt w)
      | None ->
        witness_of_diff arg (Arrow.domain fn)
        |> Option.map (fun w fmt -> pp_w "argument (not in the domain)" fmt w)
      end
    | UntypeableConstructor { expected ; given } ->
      let tuple f tys = List.map f tys |> Tuple.mk in
      let expected f = expected |> List.map (tuple f) |> Ty.disj in
      witness_of_diffs
        [ tuple GTy.lb given, expected GTy.lb ; tuple GTy.ub given, expected GTy.ub ]
      |> Option.map (fun w fmt -> pp_w "arguments" fmt w)
    | UntypeableRec { expected ; given ; _ } ->
      witness_of_diff (Tuple.mk given) (Tuple.mk expected)
      |> Option.map (fun w fmt -> pp_w "body (not of the expected type)" fmt w)
    | UntypeableProjection { arg ; domain } ->
      witness_of_diff arg domain
      |> Option.map (fun w fmt -> pp_w "argument" fmt w)
    | UntypeableCast { check ; expected ; given }
    | UntypeableCoercion { check ; expected ; given } ->
      witness_of_diffs (gty_pairs check expected given)
      |> Option.map (fun w fmt -> pp_w "expression" fmt w)
  in
  res

type style = Types | Witnesses | Both

let pp_descr ?(style=Types) fmt t =
  match style, witness_descr t with
  | Types, _ | (Witnesses | Both), None -> pp_descr fmt t
  | Witnesses, Some pp -> pp fmt
  | Both, Some pp -> Format.fprintf fmt "%a@.%t" pp_descr t pp

let descr ?style t =
  match Format.asprintf "%a" (pp_descr ?style) t with
  | "" -> None
  | d -> Some d

let pp ?style fmt t =
  match descr ?style t with
  | None -> Format.fprintf fmt "%s" (title t)
  | Some d -> Format.fprintf fmt "%s:@.%s" (title t) d
