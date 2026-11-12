open Mlsem_common
open Mlsem_types
open Mlsem_system.Annot
module SE = Mlsem_system.Error

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
| UntypeableAssignment of { var: Variable.t ; expected: Ty.t option ; given: Ty.t }

type t = {
    eid: Eid.t ;
    expr: Mlsem_system.Ast.e ;
    kind: kind ;
    env: Env.t ;
    ann: IAnnot.a
  }

exception Untypeable of t

let kind_of_system (kind:SE.kind) =
  match kind with
  | SE.UnboundVar v -> UnboundVar v
  | SE.UntypeableApp { fn ; arg } -> UntypeableApp { fn ; arg }
  | SE.UntypeableConstructor { expected ; given } -> UntypeableConstructor { expected ; given }
  | SE.UntypeableRec { funs ; expected ; given } -> UntypeableRec { funs ; expected ; given }
  | SE.UntypeableEncoding -> UntypeableEncoding
  | SE.UntypeableProjection { arg ; domain } -> UntypeableProjection { arg ; domain }
  | SE.UntypeableCast { check ; expected ; given } -> UntypeableCast { check ; expected ; given }
  | SE.UntypeableCoercion { check ; expected ; given } -> UntypeableCoercion { check ; expected ; given }
  | SE.InvalidAnnot msg -> InvalidAnnot msg

let of_system (err:SE.t) =
  let kind =
    match err.kind, MVariable.as_ref_assign err.expr with
    | SE.UntypeableApp { arg ; _ }, Some var ->
      (* The argument of the encoding is a pair (cell, assigned value) *)
      let given = Tuple.proj 2 1 arg in
      let expected = MVariable.content_ty var (Tuple.proj 2 0 arg) in
      UntypeableAssignment { var ; expected ; given }
    | kind, _ -> kind_of_system kind
  in
  { eid=err.eid ; expr=err.expr ; kind ; env=err.env ; ann=err.ann }

(* The System error whose printers describe [kind]:
   an assignment is described as a failing static check of the assigned value. *)
let to_system t =
  let kind =
    match t.kind with
    | UnboundVar v -> SE.UnboundVar v
    | UntypeableApp { fn ; arg } -> SE.UntypeableApp { fn ; arg }
    | UntypeableConstructor { expected ; given } -> SE.UntypeableConstructor { expected ; given }
    | UntypeableRec { funs ; expected ; given } -> SE.UntypeableRec { funs ; expected ; given }
    | UntypeableEncoding -> SE.UntypeableEncoding
    | UntypeableProjection { arg ; domain } -> SE.UntypeableProjection { arg ; domain }
    | UntypeableCast { check ; expected ; given } -> SE.UntypeableCast { check ; expected ; given }
    | UntypeableCoercion { check ; expected ; given } -> SE.UntypeableCoercion { check ; expected ; given }
    | InvalidAnnot msg -> SE.InvalidAnnot msg
    | UntypeableAssignment { expected ; given ; _ } ->
      let expected = Option.value expected ~default:Ty.any in
      SE.UntypeableCoercion { check=CheckStatic ; expected=GTy.mk expected ; given=GTy.mk given }
  in
  { SE.eid=t.eid ; expr=t.expr ; kind ; env=t.env ; ann=t.ann }

type style = SE.style = Types | Witnesses | Both

let title t =
  match t.kind with
  | UntypeableAssignment _ -> "untypeable assignment"
  | _ -> SE.title (to_system t)

let pp_descr ?style fmt t =
  match t.kind with
  | UntypeableAssignment { var ; expected=None ; given } ->
    Format.fprintf fmt "variable: %a@.given: @[<h>%a@]" Variable.pp var Ty.pp given
  | UntypeableAssignment { var ; expected=Some _ ; _ } ->
    Format.fprintf fmt "variable: %a@.%a" Variable.pp var (SE.pp_descr ?style) (to_system t)
  | _ -> SE.pp_descr ?style fmt (to_system t)

let descr ?style t =
  match Format.asprintf "%a" (pp_descr ?style) t with
  | "" -> None
  | d -> Some d

let pp ?style fmt t =
  match descr ?style t with
  | None -> Format.fprintf fmt "%s" (title t)
  | Some d -> Format.fprintf fmt "%s:@.%s" (title t) d
