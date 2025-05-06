open Annot
open Types.Base
open Types.Tvar
open Types
open Env
open Ast

type ('a,'b) result =
| Ok of 'a * typ
| Fail
| Subst of Subst.t list * 'b * 'b

type ('a,'b) result_seq =
| AllOk of 'a list * typ list
| OneFail of 'b list * 'b list
| OneSubst of Subst.t list * 'b list * 'b list

let rec seq (f : 'b -> Ast.t -> ('a,'b) result) (c : 'a->
  'b) (lst:('b*Ast.t) list) : ('a,'b) result_seq =
  match lst with
  | [] -> AllOk ([],[])
  | (annot,e)::lst ->
    begin match f annot e with
    | Fail -> OneFail ([], List.map fst lst)
    | Subst (ss,a,a') -> OneSubst (ss,a::(List.map fst lst),a'::(List.map fst lst))
    | Ok (a,t) ->
      begin match seq f c lst with
      | AllOk (annots, tys) -> AllOk (a::annots, t::tys)
      | OneFail (lst1, lst2) -> OneFail ((c a)::lst1, lst2)
      | OneSubst (ss, annots, annots') ->
        OneSubst (ss, (c a)::annots, (c a)::annots') 
      end
    end

let rec infer env annot (id, e) =
  let open IAnnot in
  let retry_with a = infer env a (id, e) in
  match e, annot with
  | _, A a -> Ok (a, Checker.typeof env a (id, e))
  | _, Untyp -> Fail
  | Abstract _, Infer -> retry_with (A Annot.AAbstract)
  | Const _, Infer -> retry_with (A Annot.AConst)
  | Var v, Infer ->
    let (tvs,_) = Env.find v env |> TyScheme.get in
    let s = refresh tvs in
    retry_with (A (Annot.AAx s))
  | Atom _, Infer -> retry_with (A Annot.AAtom)
  | Tag _, Infer -> retry_with (ATag Infer)
  | Tag (_, e'), ATag annot' ->
    begin match infer' env annot' e' with
    | Ok (annot', _) -> retry_with (A (Annot.ATag annot'))
    | Subst (ss,a,a') -> Subst (ss,ATag a,ATag a')
    | Fail -> Fail
    end
  | Lambda (tys,_,_), Infer ->
    let refresh_internal ty =
      let s = refresh (vars_internal ty) in
      Subst.apply s ty
    in
    let tys = List.map refresh_internal tys in
    let branches = List.map (fun ty -> ALambda (ty, Infer)) tys in
    retry_with (AInter branches)
  | Lambda (_,v,e'), ALambda (ty, annot') ->
    let env' = Env.add v (TyScheme.mk_mono ty) env in
    begin match infer' env' annot' e' with
    | Ok (annot', _) -> retry_with (A (Annot.ALambda (ty, annot')))
    | Subst (ss,a,a') -> Subst (ss,ALambda (ty, a),ALambda (ty, a'))
    | Fail -> Fail
    end
  | Ite _, Infer -> retry_with (AIte (Infer, BInfer, BInfer))
  | Ite (e0,tau,e1,e2), AIte (a0,a1,a2) ->
    begin match infer' env a0 e0 with
    | Fail -> Fail
    | Subst (ss,a,a') -> Subst (ss,AIte (a,a1,a2),AIte (a',a1,a2))
    | Ok (a0, s) ->
      begin match infer_b_seq' env [(a1,e1);(a2,e2)] s tau with
      | OneFail _ -> Fail
      | OneSubst (ss, [a1;a2], [a1';a2']) ->
        Subst (ss, AIte(A a0,a1,a2), AIte(A a0,a1',a2'))
      | AllOk ([a1;a2],_) -> retry_with (A (Annot.AIte(a0,a1,a2)))
      | _ -> assert false
      end
    end
  | App _, Infer -> retry_with (AApp (Infer, Infer))
  | App (e1, e2), AApp (a1,a2) ->
    begin match infer_seq' env [(a1,e1);(a2,e2)] with
    | OneFail _ -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2']) ->
      Subst (ss,AApp(a1,a2),AApp(a1',a2'))
    | AllOk ([a1;a2],[t1;t2]) ->
      let tv = TVar.mk None in
      let arrow = mk_arrow t2 (TVar.typ tv) in
      let ss =
        tallying_with_result (TVar.user_vars ()) tv [(t1, arrow)]
        |> List.map fst in
      Subst (ss, A (Annot.AApp(a1,a2)), Untyp)
    | _ -> assert false
    end
  | Tuple es, Infer -> retry_with (ATuple (List.map (fun _ -> Infer) es))
  | Tuple es, ATuple annots ->
    begin match infer_seq' env (List.combine annots es) with
    | OneFail _ -> Fail
    | OneSubst (ss, a, a') -> Subst (ss,ATuple a,ATuple a')
    | AllOk (annots,_) -> retry_with (A (Annot.ATuple annots))
    end
  | Cons _, Infer -> retry_with (ACons (Infer, Infer))
  | Cons (e1,e2), ACons (a1,a2) ->
    begin match infer_seq' env [(a1,e1);(a2,e2)] with
    | OneFail _ -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2']) ->
      Subst (ss,ACons(a1,a2),ACons(a1',a2'))
    | AllOk ([a1;a2],[_;t2]) ->
      let ss = tallying (TVar.user_vars ()) [(t2,list_typ)] in
      Subst (ss, A (Annot.ACons(a1,a2)), Untyp)
    | _ -> assert false
    end
  | Projection _, Infer -> retry_with (AProj Infer)
  | Projection (p,e'), AProj annot' ->
    begin match infer' env annot' e' with
    | Ok (annot', s) ->
      let tv = TVar.mk None in
      let ty = Checker.domain_of_proj p (TVar.typ tv) in
      let ss =
        tallying_with_result (TVar.user_vars ()) tv [(s, ty)]
        |> List.map fst in
      Subst (ss, A (Annot.AProj annot'), Untyp)
    | Subst (ss,a,a') -> Subst (ss,AProj a,AProj a')
    | Fail -> Fail
    end
  | RecordUpdate (_,_,None), Infer -> retry_with (AUpdate (Infer, None))
  | RecordUpdate (_,_,Some _), Infer -> retry_with (AUpdate (Infer, Some Infer))
  | RecordUpdate (e', _, None), AUpdate (annot', None) ->
    begin match infer' env annot' e' with
    | Ok (annot', s) ->
      let ss = tallying (TVar.user_vars ()) [(s,record_any)] in
      Subst (ss, A (Annot.AUpdate(annot',None)), Untyp)
    | Subst (ss,a,a') -> Subst (ss,AUpdate (a,None),AUpdate (a',None))
    | Fail -> Fail
    end
  | RecordUpdate (e1, _, Some e2), AUpdate (a1, Some a2) ->
    begin match infer_seq' env [(a1,e1);(a2,e2)] with
    | OneFail _ -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2']) ->
      Subst (ss,AUpdate(a1,Some a2),AUpdate(a1',Some a2'))
    | AllOk ([a1;a2],[s;_]) ->
      let ss = tallying (TVar.user_vars ()) [(s,record_any)] in
      Subst (ss, A (Annot.AUpdate(a1,Some a2)), Untyp)
    | _ -> assert false
    end
  | Let (tys,_,_,_), Infer ->
    retry_with (ALet (Infer, List.map (fun ty -> (ty, Infer)) tys))
  | Let (_,v,e1,e2), ALet(annot1,parts) ->
    begin match infer' env annot1 e1 with
    | Fail -> Fail
    | Subst (ss,a,a') -> Subst (ss,ALet (a,parts),ALet (a',parts))
    | Ok (annot1, s) ->
      let parts = parts |> List.filter (fun (t,_) -> disjoint s t |> not) in
      begin match infer_part_seq' env e2 v s parts with
      | OneFail _ -> Fail
      | OneSubst (ss,p,p') -> Subst (ss,ALet(A annot1,p),ALet(A annot1,p'))
      | AllOk (p,_) -> retry_with (A (Annot.ALet (annot1, p)))
      end
    end
  | TypeConstr _, Infer -> retry_with (AConstr Infer)
  | TypeCoerce _, Infer -> retry_with (ACoerce Infer)
  | TypeConstr (e', t), AConstr annot' ->
    begin match infer' env annot' e' with
    | Ok (annot', s) ->
      let ss = tallying (TVar.user_vars ()) [(s,t)] in
      Subst (ss, A (Annot.AConstr(annot')), Untyp)
    | Subst (ss,a,a') -> Subst (ss,AConstr a,AConstr a')
    | Fail -> Fail
    end
  | TypeCoerce (e', t), ACoerce annot' ->
    begin match infer' env annot' e' with
    | Ok (annot', s) ->
      let ss = tallying (TVar.user_vars ()) [(s,t)] in
      Subst (ss, A (Annot.ACoerce(annot')), Untyp)
    | Subst (ss,a,a') -> Subst (ss,ACoerce a,ACoerce a')
    | Fail -> Fail
    end
  | e, AInter lst ->
    begin match lst with
    | [] -> Fail
    | [a] -> retry_with a
    | lst ->
      begin match infer_seq' env (List.map (fun a -> (a,(id, e))) lst) with
      | OneFail (ls,rs) -> retry_with (AInter (ls@rs))
      | OneSubst (ss,a,a') -> Subst(ss,AInter(a),AInter(a'))
      | AllOk (a,_) -> retry_with (A (Annot.AInter a))
      end
    end
  | e, a ->
    Format.printf "e:@.%a@.@.a:@.%a@.@." Ast.pp_e e IAnnot.pp a ;
    assert false
and infer' env annot e =
  let mono = TVarSet.union (Env.tvars env) (TVar.user_vars ()) in
  let subst_disjoint s =
    TVarSet.inter (Subst.dom s) mono |> TVarSet.is_empty
  in
  match infer env annot e with
  | Ok (a, ty) -> Ok (a, ty)
  | Fail -> Fail
  | Subst (ss, a1, a2) when List.for_all subst_disjoint ss ->
    let branches = ss |> List.map (fun s ->
      let annot = IAnnot.substitute s a1 in
      let tvs = TVarSet.diff (IAnnot.tvars annot) mono in
      IAnnot.substitute (refresh tvs) annot
      ) in
    let annot = IAnnot.AInter (branches@[a2]) in
    infer' env annot e
  | Subst (ss, a1, a2) -> Subst (ss, a1, a2)
and infer_b' env bannot e s tau =
  match bannot with
  | IAnnot.BInfer ->
    let ss = tallying (TVar.user_vars ()) [(s,neg tau)] in
    Subst (ss, IAnnot.BSkip, IAnnot.BType Infer)
  | IAnnot.BSkip -> Ok (Annot.BSkip, empty)
  | IAnnot.BType annot ->
    begin match infer' env annot e with
    | Ok (a, ty) -> Ok (Annot.BType a, ty)
    | Subst (ss,a1,a2) -> Subst (ss,IAnnot.BType a1,IAnnot.BType a2)
    | Fail -> Fail
    end
and infer_part' env e v s (si,annot) =
  let tvs = TVarSet.diff (vars s) (TVarSet.union (Env.tvars env) (vars si)) in
  let t = TyScheme.mk tvs (cap s si) in
  let env = Env.add v t env in
  match infer' env annot e with
  | Fail -> Fail
  | Subst (ss,a,a') -> Subst (ss,(si,a),(si,a'))
  | Ok (a,ty) -> Ok ((si,a),ty)
and infer_seq' env lst = seq (infer' env) (fun a -> A a) lst
and infer_b_seq' env lst s tau =
  seq (fun b e -> infer_b' env b e s tau)
  (function Annot.BSkip -> IAnnot.BSkip | Annot.BType a -> IAnnot.BType (A a))
  lst
and infer_part_seq' env e v s lst =
  seq (fun a e -> infer_part' env e v s a)
    (fun (si,annot) -> (si, A annot))
    (lst |> List.map (fun a -> (a,e)))

let infer env e =
  match infer' env IAnnot.Infer e with
  | Fail -> None
  | Subst _ -> assert false
  | Ok (a,_) -> Some a
