open Mlsem_common
open Annot
open Ast
open Mlsem_types

type severity = Message | Notice | Warning | Error

module Diagnostic = struct
  type kind =
  | Unchecked of { check: Ast.check ; expected: GTy.t ; given: GTy.t }
  | Unreachable

  type t = { eid: Eid.t ; kind: kind }

  let severity t =
    match t.kind with
    | Unchecked _ -> Notice
    | Unreachable -> Warning

  let title t =
    match t.kind with
    | Unchecked { check=CheckStatic ; _ } -> "Unchecked dynamic coercion"
    | Unchecked _ -> "Unchecked coercion"
    | Unreachable -> "Unreachable code"

  let pp_descr fmt t =
    match t.kind with
    | Unchecked { check=CheckStatic ; expected ; given } ->
      Format.fprintf fmt "expected: @[<h>%a@]@.given: @[<h>%a@]"
        Ty.pp (GTy.ub expected) Ty.pp (GTy.ub given)
    | Unchecked { expected ; given ; _ } ->
      Format.fprintf fmt "expected: @[<h>%a@]@.given: @[<h>%a@]"
        GTy.pp expected GTy.pp given
    | Unreachable -> ()

  let descr t =
    match Format.asprintf "%a" pp_descr t with
    | "" -> None
    | d -> Some d

  let pp fmt t =
    match descr t with
    | None -> Format.fprintf fmt "%s" (title t)
    | Some d -> Format.fprintf fmt "%s:@.%s" (title t) d
end

let rec iter_ann f (id,e) a =
  let children =
    match e, a.Annot.ann with
    | _, AValue _ | _, AVar _ -> []
    | Constructor (_, es), AConstruct anns | GConstructor (_, es), AConstruct anns
      when List.length es = List.length anns ->
      List.combine es anns
    | Let (_, _, e1, e2), ALet (a1, _, anns) ->
      (e1,a1)::(List.filter_map (function (_,Some a2) -> Some (e2, a2) | (_, None) -> None) anns)
    | App (e1,e2), AApp (a1,a2,_) | Let (_, _, e1, e2), ALet' (a1, _, a2) -> [(e1,a1) ; (e2,a2)]
    | Alt (_,es), AAlt anns when List.length es = List.length anns ->
      List.combine es anns |> List.filter_map
        (function (_,None) -> None | (e,Some a) -> Some (e,a))
    | Projection (_, e), AProj a | TypeCast (e, _, _), ACast (_, a)
    | TypeCoerce (e, _, _), ACoerce (_, a) | Lambda (_, _, e), ALambda (_, a)
    | Operation (_, e), AOp (_, a,_) -> [(e,a)]
    | Ite (e, _, e1, e2), AIte (a, _, b1, b2) ->
      (e,a)::([(e1,b1);(e2,b2)] |> List.filter_map (fun (e,b) ->
        match b with Annot.BSkip -> None | BType a -> Some (e,a)))
    | LambdaRec lst, ALambdaRec anns when List.length lst = List.length anns ->
      List.combine lst anns |> List.map (fun ((_,_,e), (_, a)) -> (e,a))
    | _, AInter anns -> anns |> List.map (fun a -> ((id,e), a))
    | _, _ -> assert false
  in
  f (id,e) a ; children |> List.iter (fun (e, a) -> iter_ann f e a)

module Visited = struct
  module ESet = Eid.Set
  type t = ESet.t
  let empty = ESet.empty
  let union = ESet.union
  let union_many = List.fold_left union empty
  let add e t = ESet.union (Eid.eq_class e) t
  let mem = ESet.mem
end

let analyze e a =
  let tyof a =
    match a.Annot.cache with
    | None -> failwith "Analyzer must be called on a fully cached annotation tree."
    | Some ty -> ty
  in
  let res = ref [] in
  let visited = ref Visited.empty in
  let aux e a =
    visited := Visited.add (fst e) !visited ;
    let report kind = res := { Diagnostic.eid=fst e ; kind }::!res in
    if Eid.show_notices (fst e) then
    match snd e, a.Annot.ann with
    | TypeCoerce (_, _, (CheckStatic | NoCheck as check)), ACoerce (expected, a) ->
      let given = tyof a in
      if GTy.leq given expected |> not then
        report (Diagnostic.Unchecked { check ; expected ; given })
    | _, _ -> ()
  in
  iter_ann aux e a ;
  List.rev !res, !visited

let get_unreachable visited e =
  let res = ref [] in
  let aux e =
    if Visited.mem (fst e) visited || not (Eid.show_notices (fst e)) then true
    else (res := { Diagnostic.eid=fst e ; kind=Unreachable }::!res ; false)
  in
  iter' aux e ;
  List.rev !res
