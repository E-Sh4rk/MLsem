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
| OneFail
| OneSubst of Subst.t list * 'b list * 'b list

let rec seq (f : 'b -> Ast.t -> ('a,'b) result) (c : 'a->
  'b) (lst:('b*Ast.t) list) : ('a,'b) result_seq =
  match lst with
  | [] -> AllOk ([],[])
  | (annot,e)::lst ->
    begin match f annot e with
    | Fail -> OneFail
    | Subst (ss,a,a') -> OneSubst (ss,a::(List.map fst lst),a'::(List.map fst lst))
    | Ok (a,t) ->
      begin match seq f c lst with
      | AllOk (annots, tys) -> AllOk (a::annots, t::tys)
      | OneFail -> OneFail
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
    | Ok (annot', _) -> retry_with (A (Annot.ATag annot'))
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
      | OneFail -> Fail
      | OneSubst (ss, [a1;a2], [a1';a2']) ->
        Subst (ss, AIte(A a0,a1,a2), AIte(A a0,a1',a2'))
      | AllOk ([a1;a2],_) -> retry_with (A (Annot.AIte(a0,a1,a2)))
      | _ -> assert false
      end
    end
  | App _, Infer -> retry_with (AApp (Infer, Infer))
  | App (e1, e2), AApp (a1,a2) ->
    begin match infer_seq' env [(a1,e1);(a2,e2)] with
    | OneFail -> Fail
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
    | OneFail -> Fail
    | OneSubst (ss, a, a') -> Subst (ss,ATuple a,ATuple a')
    | AllOk (annots,_) -> retry_with (A (Annot.ATuple annots))
    end
  | Cons _, Infer -> retry_with (ACons (Infer, Infer))
  | Cons (e1,e2), ACons (a1,a2) ->
    begin match infer_seq' env [(a1,e1);(a2,e2)] with
    | OneFail -> Fail
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
    | OneFail -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2']) ->
      Subst (ss,AUpdate(a1,Some a2),AUpdate(a1',Some a2'))
    | AllOk ([a1;a2],[s;_]) ->
      let ss = tallying (TVar.user_vars ()) [(s,record_any)] in
      Subst (ss, A (Annot.AUpdate(a1,Some a2)), Untyp)
    | _ -> assert false
    end
  | Let _, _ -> failwith "TODO"
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
  | _, _ -> assert false
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
  | IAnnot.B b -> Ok (b, Checker.typeof_b env b e)
  | IAnnot.BInfer ->
    let ss = tallying (TVar.user_vars ()) [(s,neg tau)] in
    Subst (ss, IAnnot.B Annot.BSkip, IAnnot.BType Infer)
  | IAnnot.BType annot ->
    begin match infer' env annot e with
    | Ok (a, ty) -> Ok (Annot.BType a, ty)
    | Subst (ss,a1,a2) -> Subst (ss,IAnnot.BType a1,IAnnot.BType a2)
    | Fail -> Fail
    end
and infer_seq' env lst = seq (infer' env) (fun a -> A a) lst
and infer_b_seq' env lst s tau =
  seq (fun b e -> infer_b' env b e s tau) (fun b -> B b) lst

let infer env e =
  match infer' env IAnnot.Infer e with
  | Fail -> None
  | Subst _ -> assert false
  | Ok (a,_) -> Some a
