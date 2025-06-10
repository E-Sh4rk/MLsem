open Types
open Types.Base
open Types.Tvar
open Annot
open Ast
open Env
open Parsing.Variable

(* Constants *)

let typeof_const c =
  let open Parsing.Ast in
  match c with
  | Unit -> unit_typ
  | Nil -> nil_typ
  | EmptyRecord -> empty_closed_record
  | Bool true -> true_typ
  | Bool false -> false_typ
  | Int i -> interval (Some i) (Some i)
  | Float _ -> float_typ
  | Char c -> char_interval c c
  | String str -> single_string str

(* Utils *)

let domain_of_proj p ty =
  match p with
  | Parsing.Ast.Field label ->
    mk_record true [label, (false, ty)]
  | Parsing.Ast.Pi(n,i) ->
    if i >= n then empty
    else mk_tuple (List.init n (fun k -> if k=i then ty else any))
  | Parsing.Ast.Hd ->
    mk_cons ty list_typ
  | Parsing.Ast.Tl ->
    mk_cons any (cap ty list_typ)
  | Parsing.Ast.PiTag tag ->
    mk_tag tag ty

let proj p ty =
  let open Parsing.Ast in
  match p with
  | Field label -> get_field ty label
  | Pi (n,i) -> pi n i ty
  | Hd -> destruct_list ty |> fst
  | Tl -> destruct_list ty |> snd
  | PiTag tag -> destruct_tag tag ty

let tv_proj = TVar.mk None
let typeof_proj p =
  let tv = TVar.typ tv_proj in
  let dom = domain_of_proj p tv in
  let ty = mk_arrow dom tv in
  TyScheme.mk (TVarSet.construct [tv_proj]) ty

(* Expressions *)

exception Untypeable of Parsing.Ast.exprid * string

let untypeable id msg = raise (Untypeable (id, msg))

let rec is_value (_,e) =
  match e with
  | Const _ | Atom _ | Lambda _ | Abstract _ -> true
  | Tag (_, e) -> is_value e
  | LambdaRec lst -> List.for_all (fun (_,_,e) -> is_value e) lst
  | Tuple es -> List.for_all is_value es
  | Cons (e1, e2) -> is_value e1 && is_value e2
  | _ -> false

let generalize ~e env s =
  if not (!Config.value_restriction) || is_value e then
    TyScheme.mk_poly_except (Env.tvars env) s |> TyScheme.bot_instance
  else
    TyScheme.mk_mono s

let rec typeof' env annot (id,e) =
  let open Annot in
  match e, annot with
  | Abstract _, AAbstract ty -> ty
  | Const c, AConst -> typeof_const c
  | Var v, AAx s ->
    if Env.mem v env then begin
      let (tvs, ty) = Env.find v env |> TyScheme.get in
      if TVarSet.subset (Subst.dom s) tvs then
        Subst.apply s ty
      else
        untypeable id ("Invalid substitution for "^(Variable.show v)^".")
    end else
      untypeable id ("Undefined variable "^(Variable.show v)^".")
  | Atom a, AAtom -> mk_atom a
  | Tag (tag, e), ATag annot -> mk_tag tag (typeof env annot e)
  | Lambda (_, v, e), ALambda (s, annot) ->
    let env = Env.add v (TyScheme.mk_mono s) env in
    let t = typeof env annot e in
    mk_arrow s t
  | LambdaRec lst, ALambdaRec anns when List.length lst = List.length anns ->
    let lst = List.combine lst anns in
    let env = lst |> List.fold_left
      (fun env ((_,v,_),(ty,_)) -> Env.add v (TyScheme.mk_mono ty) env) env in
    let tys = lst |> List.map (fun ((_,_,e),(ty,annot)) -> typeof env annot e, ty) in
    if List.for_all (fun (ty, ty') -> subtype ty ty') tys
    then tys |> List.map fst |> mk_tuple
    else untypeable id ("Invalid recursive lambda.")
  | Ite (e, tau, e1, e2), AIte (annot, b1, b2) ->
    let s = typeof env annot e in
    let t1 = typeof_b env b1 e1 s tau in
    let t2 = typeof_b env b2 e2 s (neg tau) in
    cup t1 t2
  | ControlFlow (_, e, tau, e1, e2), ACf (annot, b1, b2) ->
    let s = typeof env annot e in
    let _ = typeof_b env b1 e1 s tau in
    let _ = typeof_b env b2 e2 s (neg tau) in
    (* if subtype t1 unit_typ && subtype t2 unit_typ
    then unit_typ
    else untypeable id "Invalid control flow: branches should have unit type." *)
    unit_typ
  | App (e1, e2), AApp (annot1, annot2) ->
    let t1 = typeof env annot1 e1 in
    let t2 = typeof env annot2 e2 in
    if subtype t1 arrow_any then
      if subtype t2 (domain t1)
      then apply t1 t2
      else untypeable id "Invalid application: argument not in the domain."
    else untypeable id "Invalid application: not a function."
  | Tuple es, ATuple annots when List.length es = List.length annots ->
    List.combine es annots |> List.map (fun (e, annot) ->
      typeof env annot e
    ) |> mk_tuple
  | Cons (e1, e2), ACons (annot1, annot2) ->
    let t1 = typeof env annot1 e1 in
    let t2 = typeof env annot2 e2 in
    if subtype t2 list_typ
    then mk_cons t1 t2
    else untypeable id "Invalid cons: not a list."
  | Projection (p, e), AProj annot ->
    let t = typeof env annot e in
    if subtype t (domain_of_proj p any)
    then proj p t
    else untypeable id "Invalid projection."
  | RecordUpdate (e, label, None), AUpdate (annot, None) ->
    let t = typeof env annot e in
    if subtype t record_any
    then remove_field t label
    else untypeable id "Invalid field deletion: not a record."
  | RecordUpdate (e, label, Some e'), AUpdate (annot, Some annot') ->
    let t = typeof env annot e in
    if subtype t record_any
    then
      let t' = typeof env annot' e' in
      let right_record = mk_record false [label, (false, t')] in
      merge_records t right_record  
    else untypeable id "Invalid field update: not a record."
  | Let (_, v, e1, e2), ALet (annot1, annots2) ->
    let tvs,s = typeof_def env annot1 e1 |> TyScheme.get in
    let aux (si, annot) =
      if TVarSet.inter tvs (vars si) |> TVarSet.is_empty then
        let s = TyScheme.mk tvs (cap s si) in
        typeof (Env.add v s env) annot e2
      else
        untypeable id ("Partition of "^(Variable.show v)^" contains generalized variables.")
    in
    if subtype s (List.map fst annots2 |> disj) then
      List.map aux annots2 |> disj
    else
      untypeable id ("Partition does not cover the type of "^(Variable.show v)^".")
  | TypeConstr (e, ty), AConstr annot ->
    let t = typeof env annot e in
    if subtype t ty then t
    else untypeable id "Type constraint not satisfied."
  | TypeCoerce (e, _), ACoerce (ty, annot) ->
    let t = typeof env annot e in
    if subtype t ty then ty
    else untypeable id "Impossible type coercion."
  | e, AInter lst ->
    lst |> List.map (fun a -> typeof env a (id,e)) |> conj
  | e, a ->
    Format.printf "e:@.%a@.@.a:@.%a@.@." Ast.pp_e e Annot.pp_a a ;
    assert false
and typeof env annot e =
  match annot.cache with
  | Some ty -> ty
  | None ->
    let ty = typeof' env annot.ann e |> normalize in
    annot.cache <- Some ty ;
    ty
and typeof_b env bannot (id,e) s tau =
  match bannot with
  | BType annot -> typeof env annot (id,e)
  | BSkip ->
    if disjoint s tau |> not
    then untypeable id "Branch is reachable and must be typed." ;
    empty
and typeof_def env annot e =
  typeof env annot e |> generalize ~e env
