open Types
open Types.Base
open Types.Tvar
open Annot
open Ast
open Env
open Variable
open Types.Gradual

(* Projections and constructors *)

let domain_of_proj p ty =
  match p with
  | Field label ->
    mk_record true [label, (false, ty)]
  | Pi(n,i) ->
    if i >= n then empty
    else mk_tuple (List.init n (fun k -> if k=i then ty else any))
  | Hd ->
    mk_cons ty list_typ
  | Tl ->
    mk_cons any (cap ty list_typ)
  | PiTag tag ->
    mk_tag tag ty

let proj p ty =
  match p with
  | Field label -> get_field ty label
  | Pi (n,i) -> pi n i ty
  | Hd -> destruct_list ty |> fst
  | Tl -> destruct_list ty |> snd
  | PiTag tag -> destruct_tag tag ty

let domains_of_construct (c:Ast.constructor) =
  match c with
  | Tuple n -> List.init n (fun _ -> any)
  | Cons -> [ any ; list_typ ]
  | RecUpd _ -> [ record_any ; any ]
  | RecDel _ -> [ record_any ]
  | Tag _ -> [ any ]
  | Atom _ -> []

let construct (c:Ast.constructor) tys =
  match c, tys with
  | Tuple n, tys when List.length tys = n -> mk_tuple tys
  | Cons, [t1 ; t2] -> mk_cons t1 t2
  | RecUpd lbl, [t1 ; t2] ->
    let right_record = mk_record false [lbl, (false, t2)] in
    merge_records t1 right_record
  | RecDel lbl, [t] -> remove_field t lbl
  | Tag tag, [t] -> mk_tag tag t
  | Atom atom, [] -> mk_atom atom
  | _ -> failwith "Invalid arity for constructor."

(* Expressions *)

type error = { eid: Eid.t ; title: string ; descr: string option }
exception Untypeable of error

let untypeable id msg = raise (Untypeable { eid=id ; title=msg ; descr=None })

let rec is_value (_,e) =
  match e with
  | Const _ | Lambda _ | Abstract _ -> true
  | Constructor (_, es) -> List.for_all is_value es
  | LambdaRec lst -> List.for_all (fun (_,_,e) -> is_value e) lst
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
  | Const c, AConst -> typeof_const c |> GTy.mk
  | Var v, AAx s ->
    if Env.mem v env then begin
      let (tvs, ty) = Env.find v env |> TyScheme.get in
      if TVarSet.subset (Subst.dom s) tvs then
        GTy.substitute s ty
      else
        untypeable id ("Invalid substitution for "^(Variable.show v)^".")
    end else
      untypeable id ("Undefined variable "^(Variable.show v)^".")
  | Constructor (c, es), AConstruct annots when List.length es = List.length annots ->
    let doms = domains_of_construct c in
    if List.length doms = List.length es then
      let check tys = List.for_all2 subtype tys doms in
      let tys = List.map2 (fun e a -> typeof env a e) es annots in
      begin match GTy.opl check (construct c) tys with
      | Some ty -> ty
      | None -> untypeable id ("Invalid domain for constructor.")
      end
    else
      untypeable id ("Invalid arity for constructor.")
  | Lambda (_, v, e), ALambda (s, annot) ->
    let env = Env.add v (TyScheme.mk_mono s) env in
    let t = typeof env annot e in
    let lb = mk_arrow (GTy.ub s) (GTy.lb t) in
    let ub = mk_arrow (GTy.lb s) (GTy.ub t) in
    GTy.mk_gradual lb ub
  | LambdaRec lst, ALambdaRec anns when List.length lst = List.length anns ->
    let lst = List.combine lst anns in
    let env = lst |> List.fold_left
      (fun env ((_,v,_),(ty,_)) -> Env.add v (TyScheme.mk_mono ty) env) env in
    let tys = lst |> List.map (fun ((_,_,e),(ty,annot)) -> typeof env annot e, ty) in
    if List.for_all (fun (ty, ty') -> subtype (GTy.lb ty) (GTy.lb ty')) tys
    then tys |> List.map fst |> GTy.mapl mk_tuple
    else untypeable id ("Invalid recursive lambda.")
  | Ite (e, tau, e1, e2), AIte (annot, b1, b2) ->
    let s = typeof env annot e in
    let t1 = typeof_b env b1 e1 s tau in
    let t2 = typeof_b env b2 e2 s (neg tau) in
    GTy.cup t1 t2
  | ControlFlow (_, e, tau, e1, e2), ACf (annot, b1, b2) ->
    let s = typeof env annot e in
    let _ = typeof_b env b1 e1 s tau in
    let _ = typeof_b env b2 e2 s (neg tau) in
    GTy.mk unit_typ
  | App (e1, e2), AApp (annot1, annot2) ->
    let check ty1 ty2 =
      subtype ty1 arrow_any &&
      subtype ty2 (domain ty1)
    in
    let t1 = typeof env annot1 e1 in
    let t2 = typeof env annot2 e2 in
    begin match GTy.op2 check apply t1 t2 with
    | Some ty -> ty
    | None -> untypeable id "Invalid application."
    end
  | Projection (p, e), AProj annot ->
    let dom = domain_of_proj p any in
    let check ty = subtype ty dom in
    let t = typeof env annot e in
    begin match GTy.op check (proj p) t with
    | Some ty -> ty
    | None -> untypeable id "Invalid projection."
    end
  | Let (_, v, e1, e2), ALet (annot1, annots2) ->
    let tvs,s = typeof_def env annot1 e1 |> TyScheme.get in
    let aux (si, annot) =
      let si = GTy.mk si in
      if TVarSet.inter tvs (GTy.fv si) |> TVarSet.is_empty then
        let s = TyScheme.mk tvs (GTy.cap s si) in
        typeof (Env.add v s env) annot e2
      else
        untypeable id ("Partition of "^(Variable.show v)^" contains generalized variables.")
    in
    if subtype (GTy.ub s) (List.map fst annots2 |> disj) then
      List.map aux annots2 |> GTy.disj
    else
      untypeable id ("Partition does not cover the type of "^(Variable.show v)^".")
  | TypeCast (e, ty), AConstr annot ->
    let t = typeof env annot e in
    if subtype (GTy.lb t) ty then t
    else untypeable id "Type constraint not satisfied."
  | TypeCoerce (e, _), ACoerce (ty, annot) ->
    let t = typeof env annot e in
    if GTy.leq t ty then ty
    else untypeable id "Impossible type coercion."
  | e, AInter lst ->
    lst |> List.map (fun a -> typeof env a (id,e)) |> GTy.conj
  | e, a ->
    Format.printf "e:@.%a@.@.a:@.%a@.@." Ast.pp_e e Annot.pp_a a ;
    assert false
and typeof env annot e =
  match annot.cache with
  | Some ty -> ty
  | None ->
    let ty = typeof' env annot.ann e |> GTy.normalize in
    annot.cache <- Some ty ;
    ty
and typeof_b env bannot (id,e) s tau =
  match bannot with
  | BType annot -> typeof env annot (id,e)
  | BSkip ->
    if disjoint (GTy.ub s) tau |> not
    then untypeable id "Branch is reachable and must be typed." ;
    GTy.empty
and typeof_def env annot e =
  typeof env annot e |> generalize ~e env
