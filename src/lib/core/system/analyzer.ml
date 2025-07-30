open Common
open Annot
open Ast
open Types

type severity = Message | Notice | Warning | Error
type msg = { eid: Eid.t ; severity: severity ; title: string ; descr: string option }

let rec iter_ann f (id,e) a =
  let children =
    match e, a.Annot.ann with
    | _, AConst | _, AAbstract _ | _, AAx _ -> []
    | Constructor (_, es), AConstruct anns when List.length es = List.length anns ->
      List.combine es anns
    | Let (_, _, e1, e2), ALet (a1, anns) ->
      (e1,a1)::(List.map (fun (_,a2) -> (e2, a2)) anns)
    | App (e1,e2), AApp (a1,a2) -> [(e1,a1) ; (e2,a2)]
    | Projection (_, e), AProj a | TypeCast (e, _), ACast a
    | TypeCoerce (e, _, _), ACoerce (_, a) | Lambda (_, _, e), ALambda (_, a) -> [(e,a)]
    | Ite (e, _, e1, e2), AIte (a, b1, b2) | ControlFlow (_, e, _, e1, e2), ACf (a, b1, b2) ->
      (e,a)::([(e1,b1);(e2,b2)] |> List.filter_map (fun (e,b) ->
        match b with Annot.BSkip -> None | BType a -> Some (e,a)))
    | LambdaRec lst, ALambdaRec anns when List.length lst = List.length anns ->
      List.combine lst anns |> List.map (fun ((_,_,e), (_, a)) -> (e,a))
    | _, AInter anns -> anns |> List.map (fun a -> ((id,e), a))
    | _, _ -> assert false
  in
  f (id,e) a ; children |> List.iter (fun (e, a) -> iter_ann f e a)

let analyze e a =
  let tyof a =
    match a.Annot.cache with
    | None -> failwith "Analyzer must be called on a fully cached annotation tree."
    | Some ty -> ty
  in
  let res = ref [] in
  let msg m = res := m::!res in
  let aux e a =
    let msg s t d = msg { eid=fst e ; severity=s ; title=t ; descr=Some d } in
    match snd e, a.Annot.ann with
    | TypeCoerce (_, _, c), ACoerce (ty, a) ->
      let s = tyof a in
      if GTy.leq s ty |> not then
        begin if c = CheckStatic then
          msg Notice "Unchecked dynamic coercion"
          (Format.asprintf "expected: %a\ngiven: %a" Ty.pp (GTy.ub ty) Ty.pp (GTy.ub s))
        else if c = NoCheck then
          msg Notice "Unchecked coercion"
          (Format.asprintf "expected: %a\ngiven: %a" GTy.pp ty GTy.pp s)
        end
    | _, _ -> ()
  in
  iter_ann aux e a ;
  List.rev !res
