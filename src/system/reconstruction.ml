open Annot
open Types.Base
open Types.Tvar
open Types
open Env
open Ast
open Caching

(* Auxiliary *)

let tsort leq lst =
  let rec add_elt lst ne =
    match lst with
    | [] -> [ne]
    | e::lst when leq ne e -> ne::e::lst
    | e::lst -> e::(add_elt lst ne)
  in
  List.fold_left add_elt [] (List.rev lst)

let minimize_new_tvars tvars sol v =
  let t = Subst.find sol v in
  let vs = TVarSet.diff (top_vars t) tvars |> TVarSet.filter TVar.internal in
  let s = vars_with_polarity t |> List.filter_map (fun (v', k) ->
    if TVarSet.mem vs v' then
    match k with
    | `Pos -> Some (v', TVar.typ v)
    | `Neg -> Some (v', TVar.typ v |> neg)
    | `Both -> (* Cases like Bool & 'a \ 'b  |  Int & 'a & 'b *) None
    else None
    ) |> Subst.construct
  in
  Subst.compose s sol
let minimize_new_tvars tvars sol =
  List.fold_left (minimize_new_tvars tvars) sol (Subst.dom sol |> TVarSet.destruct)

let tallying_no_result env cs =
  let tvars = Env.tvars env in
  tallying_with_prio (TVar.user_vars ()) (tvars |> TVarSet.destruct) cs
  |> List.map (minimize_new_tvars tvars)
  |> List.map (fun s -> s, empty)

let tallying_with_result env res cs =
  let tvars = Env.tvars env in
  let leq_sol (_,r1) (_,r2) = subtype r1 r2 in
  (* Format.printf "Tallying:@." ;
  cs |> List.iter (fun (a,b) -> Format.printf "%a <= %a@." pp_typ a pp_typ b) ; *)
  tallying_with_prio (TVar.user_vars ()) (tvars |> TVarSet.destruct) cs
  |> List.map (minimize_new_tvars tvars)
  |> List.map (fun s -> s, Subst.apply s res)
  (* Simplify result if it does not impact the domains *)
  |> List.map (fun (s,r) ->
    let mono = Subst.restrict s tvars |> Subst.vars in
    let mono = TVarSet.union_many [mono ; tvars ; TVar.user_vars ()] in
    let clean = clean_subst ~pos:empty ~neg:any mono r in
    (Subst.compose clean s, Subst.apply clean r)
  )
  |> tsort leq_sol
  (* |> List.map (fun (s,r) -> Format.printf "%a@.%a@." Subst.pp s pp_typ r ; s,r) *)

let reduce_parts tvs s parts =
  let open IAnnot in
  let parts = parts |> List.filter (fun (t,_) -> disjoint s t |> not) in
  let mono, s = TyScheme.mk tvs s |> TyScheme.get_fresh in
  let mono = TVarSet.filter TVar.from_user mono in
  (* The first time, we merge parts that will already be discriminated by future tallying instances *)
  let rec aux parts =
    match parts with
    | (a,Infer)::(b,Infer)::parts ->
      let mono = TVarSet.union mono (vars a) in
      if test_tallying mono [ (s,a) ; (a,s) ]
      then aux ((cup a b, Infer)::parts)
      else (a,Infer)::(aux ((b,Infer)::parts))
    | parts -> parts
  in
  aux parts

(* Reconstruction algorithm *)

type ('a,'b) result =
| Ok of 'a * typ
| Fail
| Subst of (Subst.t * typ) list * 'b * 'b * (Parsing.Ast.exprid * REnv.t)

type ('a,'b) result_seq =
| AllOk of 'a list * typ list
| OneFail
| OneSubst of (Subst.t * typ) list * 'b list * 'b list * (Parsing.Ast.exprid * REnv.t)

type cache = { dom : Domain.t ; cache : ((Annot.t, IAnnot.t) result) Cache.t ; tvcache : TVCache.t }

let rec seq (f : 'b -> 'c -> ('a,'b) result) (c : 'a->'b) (lst:('b*'c) list)
  : ('a,'b) result_seq =
  match lst with
  | [] -> AllOk ([],[])
  | (annot,e)::lst ->
    begin match f annot e with
    | Fail -> OneFail
    | Subst (ss,a,a',r) -> OneSubst (ss,a::(List.map fst lst),a'::(List.map fst lst),r)
    | Ok (a,t) ->
      begin match seq f c lst with
      | AllOk (annots, tys) -> AllOk (a::annots, t::tys)
      | OneFail -> OneFail
      | OneSubst (ss, annots, annots',r) ->
        OneSubst (ss, (c a)::annots, (c a)::annots',r) 
      end
    end

let nc a = IAnnot.A (Annot.nc a)

let rec infer cache env annot (id, e) =
  let open IAnnot in
  let retry_with a = infer cache env a (id, e) in
  let empty_cov = (id, REnv.empty) in
  match e, annot with
  | _, A a -> Ok (a, Checker.typeof env a (id, e))
  | _, Untyp -> Fail
  | Abstract ty, Infer -> retry_with (nc (Annot.AAbstract ty))
  | Const _, Infer -> retry_with (nc Annot.AConst)
  | Var v, Infer when Env.mem v env ->
    let (tvs,_) = Env.find v env |> TyScheme.get in
    let s = TVCache.get' cache.tvcache id tvs in
    retry_with (nc (Annot.AAx s))
  | Var _, Infer -> Fail
  | Atom _, Infer -> retry_with (nc Annot.AAtom)
  | Tag _, Infer -> retry_with (ATag Infer)
  | Tag (_, e'), ATag annot' ->
    begin match infer' cache env annot' e' with
    | Ok (annot', _) -> retry_with (nc (Annot.ATag annot'))
    | Subst (ss,a,a',r) -> Subst (ss,ATag a,ATag a',r)
    | Fail -> Fail
    end
  | Lambda (d,_,_), Infer -> retry_with (ALambda (d, Infer))
  | Lambda (_,v,e'), ALambda (ty, annot') ->
    let env' = Env.add v (TyScheme.mk_mono ty) env in
    begin match infer' { cache with dom=Domain.empty } env' annot' e' with
    | Ok (annot', _) -> retry_with (nc (Annot.ALambda (ty, annot')))
    | Subst (ss,a,a',(eid,r)) ->
      Subst (ss,ALambda (ty, a),ALambda (ty, a'),(eid,REnv.add v ty r))
    | Fail -> Fail
    end
  | LambdaRec lst, Infer ->
    retry_with (ALambdaRec (List.map (fun (ty,_,_) -> ty, Infer) lst))
  | LambdaRec lst, ALambdaRec anns ->
    let lst = List.combine lst anns in
    let env' = lst |> List.fold_left
      (fun env ((_,v,_),(ty,_)) -> Env.add v (TyScheme.mk_mono ty) env) env in
    let tys = List.map fst anns in
    begin match infer_seq' cache env' (List.map (fun ((_,_,e),(_,a)) -> a,e) lst) with
    | OneFail -> Fail
    | OneSubst (ss, a, a',r) ->
      Subst (ss,ALambdaRec (List.combine tys a),ALambdaRec (List.combine tys a'),r)
    | AllOk (annots,tys') ->
      let cs = List.combine tys' tys in
      let ss = tallying_with_result env (mk_tuple tys') cs in
      let ok_ann = nc (Annot.ALambdaRec (List.combine tys annots)) in
      Subst (ss, ok_ann, Untyp, empty_cov)
    end
  | Ite _, Infer -> retry_with (AIte (Infer, BInfer, BInfer))
  | Ite (e0,tau,e1,e2), AIte (a0,a1,a2) ->
    let to_i =
      (function Annot.BSkip -> IAnnot.BSkip | Annot.BType a -> IAnnot.BType (A a)) in
    begin match infer' cache env a0 e0 with
    | Fail -> Fail
    | Subst (ss,a,a',r) -> Subst (ss,AIte (a,a1,a2),AIte (a',a1,a2),r)
    | Ok (a0, s) ->
      begin match infer_b' cache env a1 e1 s tau with
      | Fail -> Fail
      | Subst (ss, a1, a1',r) ->
        Subst (ss, AIte(A a0,a1,a2), AIte(A a0,a1',a2),r)
      | Ok (a1,_) ->
        begin match infer_b' cache env a2 e2 s (neg tau) with
        | Fail -> Fail
        | Subst (ss, a2, a2',r) ->
          Subst (ss, AIte(A a0,to_i a1,a2), AIte(A a0,to_i a1,a2'),r)
        | Ok (a2,_) -> retry_with (nc (Annot.AIte(a0,a1,a2)))
        end  
      end
    end
  | App _, Infer -> retry_with (AApp (Infer, Infer))
  | App (e1, e2), AApp (a1,a2) ->
    begin match infer_seq' cache env [(a1,e1);(a2,e2)] with
    | OneFail -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2'],r) ->
      Subst (ss,AApp(a1,a2),AApp(a1',a2'),r)
    | AllOk ([a1;a2],[t1;t2]) ->
      let tv = TVCache.get cache.tvcache id TVCache.res_tvar in
      let arrow = mk_arrow t2 (TVar.typ tv) in
      let ss = tallying_with_result env (TVar.typ tv) [(t1, arrow)] in
      Subst (ss, nc (Annot.AApp(a1,a2)), Untyp, empty_cov)
    | _ -> assert false
    end
  | Tuple es, Infer -> retry_with (ATuple (List.map (fun _ -> Infer) es))
  | Tuple es, ATuple annots ->
    begin match infer_seq' cache env (List.combine annots es) with
    | OneFail -> Fail
    | OneSubst (ss, a, a',r) -> Subst (ss,ATuple a,ATuple a',r)
    | AllOk (annots,_) -> retry_with (nc (Annot.ATuple annots))
    end
  | Cons _, Infer -> retry_with (ACons (Infer, Infer))
  | Cons (e1,e2), ACons (a1,a2) ->
    begin match infer_seq' cache env [(a1,e1);(a2,e2)] with
    | OneFail -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2'],r) ->
      Subst (ss,ACons(a1,a2),ACons(a1',a2'),r)
    | AllOk ([a1;a2],[_;t2]) ->
      let ss = tallying_no_result env [(t2,list_typ)] in
      Subst (ss, nc (Annot.ACons(a1,a2)), Untyp, empty_cov)
    | _ -> assert false
    end
  | Projection _, Infer -> retry_with (AProj Infer)
  | Projection (p,e'), AProj annot' ->
    begin match infer' cache env annot' e' with
    | Ok (annot', s) ->
      let tv = TVCache.get cache.tvcache id TVCache.res_tvar in
      let ty = Checker.domain_of_proj p (TVar.typ tv) in
      let ss = tallying_with_result env (TVar.typ tv) [(s, ty)] in
      Subst (ss, nc (Annot.AProj annot'), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,AProj a,AProj a',r)
    | Fail -> Fail
    end
  | RecordUpdate (_,_,None), Infer -> retry_with (AUpdate (Infer, None))
  | RecordUpdate (_,_,Some _), Infer -> retry_with (AUpdate (Infer, Some Infer))
  | RecordUpdate (e', _, None), AUpdate (annot', None) ->
    begin match infer' cache env annot' e' with
    | Ok (annot', s) ->
      let ss = tallying_no_result env [(s,record_any)] in
      Subst (ss, nc (Annot.AUpdate(annot',None)), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,AUpdate (a,None),AUpdate (a',None),r)
    | Fail -> Fail
    end
  | RecordUpdate (e1, _, Some e2), AUpdate (a1, Some a2) ->
    begin match infer_seq' cache env [(a1,e1);(a2,e2)] with
    | OneFail -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2'],r) ->
      Subst (ss,AUpdate(a1,Some a2),AUpdate(a1',Some a2'),r)
    | AllOk ([a1;a2],[s;_]) ->
      let ss = tallying_no_result env [(s,record_any)] in
      Subst (ss, nc (Annot.AUpdate(a1,Some a2)), Untyp, empty_cov)
    | _ -> assert false
    end
  | Let (tys,_,_,_), Infer ->
    retry_with (ALet (Infer, List.map (fun ty -> (ty, Infer)) tys))
  | Let (_,v,e1,e2), ALet(annot1,parts) ->
    begin match infer' cache env annot1 e1 with
    | Fail -> Fail
    | Subst (ss,a,a',r) -> Subst (ss,ALet (a,parts),ALet (a',parts),r)
    | Ok (annot1, s) ->
      let tvs, s = Checker.generalize ~e:e1 env s |> TyScheme.get in
      let parts = reduce_parts tvs s parts in
      begin match infer_part_seq' cache env e2 v (tvs,s) parts with
      | OneFail -> Fail
      | OneSubst (ss,p,p',r) -> Subst (ss,ALet(A annot1,p),ALet(A annot1,p'),r)
      | AllOk (p,_) -> retry_with (nc (Annot.ALet (annot1, p)))
      end
    end
  | TypeConstr _, Infer -> retry_with (AConstr Infer)
  | TypeCoerce (_,t), Infer -> retry_with (ACoerce (t,Infer))
  | TypeConstr (e', t), AConstr annot' ->
    begin match infer' cache env annot' e' with
    | Ok (annot', s) ->
      let ss = tallying_no_result env [(s,t)] in
      Subst (ss, nc (Annot.AConstr(annot')), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,AConstr a,AConstr a',r)
    | Fail -> Fail
    end
  | TypeCoerce (e', _), ACoerce (t,annot') ->
    begin match infer' cache env annot' e' with
    | Ok (annot', s) ->
      let ss = tallying_no_result env [(s,t)] in
      Subst (ss, nc (Annot.ACoerce(t,annot')), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,ACoerce (t,a),ACoerce (t,a'),r)
    | Fail -> Fail
    end
  | e, AInter lst ->
    let rec aux dom lst =
      match lst with
      | [] -> Either.left []
      | { coverage ; ann }::lst ->
        let dom', useless =
          match coverage with
          | None -> dom, false
          | Some cov -> Domain.add cov dom, Domain.covers dom cov
        in
        if useless then aux dom lst
        else
          begin match infer' {cache with dom} env ann (id,e) with
          | Fail -> aux dom lst
          | Subst (ss,a,a',r) ->
            let a, a' = { coverage ; ann=a }, { coverage ; ann=a' } in
            Either.right (ss,a::lst,a'::lst,r)
          | Ok (a,_) ->
            begin match aux dom' lst with
            | Either.Left lst -> Either.Left (a::lst)
            | Either.Right (ss,lst,lst',r) ->
              let c = { coverage ; ann=A a } in
              Either.Right (ss,c::lst,c::lst',r)
            end
          end
    in
    begin match aux cache.dom lst with
    | Either.Left [] -> Fail
    | Either.Left [a] -> retry_with (A a)
    | Either.Left lst -> retry_with (nc (Annot.AInter lst))
    | Either.Right (ss,a,a',r) -> Subst (ss,AInter(a),AInter(a'),r)
    end
  | e, a ->
    Format.printf "e:@.%a@.@.a:@.%a@.@." Ast.pp_e e IAnnot.pp a ;
    assert false
and infer_nc' cache env annot e =
  let mono = TVarSet.union (Env.tvars env) (TVar.user_vars ()) in
  let subst_disjoint s =
    TVarSet.inter (Subst.dom s) mono |> TVarSet.is_empty
  in
  match infer cache env annot e with
  | Ok (a, ty) -> Ok (a, ty)
  | Fail -> Fail
  | Subst (ss, a1, a2, (eid,r)) when ss |> List.map fst |> List.for_all subst_disjoint ->
    let branches = ss |> List.map (fun (s,ty) ->
      let ann = IAnnot.substitute s a1 in
      let coverage = (Some (eid, ty), REnv.substitute s r) in
      { IAnnot.coverage=(Some coverage) ; ann }
      ) in
    let coverage = Some (None, r) in
    let annot = IAnnot.AInter (branches@[{ coverage ; ann=a2 }]) in
    infer' cache env annot e
  | Subst (ss, a1, a2, r) -> Subst (ss, a1, a2, r)
and infer' cache env annot e =
  match Cache.get e env annot cache.cache with
  | Some r -> r
  | None ->
    let r = infer_nc' cache env annot e in
    Cache.add e env annot r cache.cache ; r
and infer_b' cache env bannot e s tau =
  let empty_cov = (fst e, REnv.empty) in
  match bannot with
  | IAnnot.BInfer ->
    let ss = tallying_no_result env [(s,neg tau)] in
    Subst (ss, IAnnot.BSkip, IAnnot.BType Infer, empty_cov)
  | IAnnot.BSkip -> Ok (Annot.BSkip, empty)
  | IAnnot.BType annot ->
    begin match infer' cache env annot e with
    | Ok (a, ty) -> Ok (Annot.BType a, ty)
    | Subst (ss,a1,a2,r) -> Subst (ss,IAnnot.BType a1,IAnnot.BType a2,r)
    | Fail -> Fail
    end
and infer_part' cache env e v (tvs, s) (si,annot) =
  let t = TyScheme.mk tvs (cap s si) in
  let env = Env.add v t env in
  match infer' cache env annot e with
  | Fail -> Fail
  | Subst (ss,a,a',r) -> Subst (ss,(si,a),(si,a'),r)
  | Ok (a,ty) -> Ok ((si,a),ty)
and infer_seq' cache env lst = seq (infer' cache env) (fun a -> A a) lst
and infer_part_seq' cache env e v s lst =
  seq (fun a () -> infer_part' cache env e v s a)
    (fun (si,annot) -> (si, A annot))
    (lst |> List.map (fun a -> (a,())))

let infer env e =
  let cache = { dom = Domain.empty ; cache = Cache.empty () ; tvcache = TVCache.empty () } in
  match infer' cache env IAnnot.Infer e with
  | Fail -> None
  | Subst _ -> failwith "Top-level environment should not contain an unresolved type variable."
  | Ok (a,_) -> Some a
