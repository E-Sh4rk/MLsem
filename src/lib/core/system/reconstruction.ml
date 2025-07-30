open Common
open Annot
open Types
open Ast
open Caching

type ('a,'b) result =
| Ok of 'a * GTy.t
| Fail
| Subst of (Subst.t * Ty.t) list * 'b * 'b * (Eid.t * REnv.t)

type log = { eid: Eid.t ; title: string ; descr: Format.formatter -> unit }
type cache = { dom : Domain.t ; tvcache : TVCache.t ; logs : log list ref }

(* Auxiliary *)

let tsort leq lst =
  let rec add_elt lst ne =
    match lst with
    | [] -> [ne]
    | e::lst when leq ne e -> ne::e::lst
    | e::lst -> e::(add_elt lst ne)
  in
  List.fold_left add_elt [] (List.rev lst)

let substitute_by_similar_var v t =
  let vs = TVarSet.rm v (top_vars t) in
  let nt = vars_with_polarity t |> List.find_map (fun (v', k) ->
    if TVarSet.mem vs v' then
    match k with
    | `Pos -> Some (TVar.typ v')
    | `Neg -> Some (TVar.typ v' |> Ty.neg)
    | `Both -> (* Cases like Bool & 'a \ 'b  |  Int & 'a & 'b *) None
    else None
    )
  in
  match nt with
  | None -> Subst.identity
  | Some nt -> Subst.construct [(v,nt)]

let abstract_factors cache v ty =
  let (factor, _) = factorize (TVarSet.construct [v], TVarSet.empty) ty in
  let res = ref [] in
  let aux (abs, dnf) =
    let vs = Abstract.params abs in
    let mk_arg i (ty,variance) =
      let tv = TVCache.get_abs_param cache.tvcache abs i v in
      let arg = match variance with
      | Abstract.Inv -> ty | Abstract.Cov -> Ty.cap ty (TVar.typ tv) | Abstract.Cav -> Ty.cup ty (TVar.typ tv)
      in
      let s = substitute_by_similar_var tv arg in
      Subst.apply s arg
    in
    let mk_abs tys =
      let args = List.combine tys vs |> List.mapi mk_arg in
      Abstract.mk abs args
    in
    dnf |> List.filter (fun (ps, ns) ->
      if ps = [] then true
      else
        let ps = ps |> List.map mk_abs |> Ty.conj in
        let ns = ns |> List.map (Abstract.mk abs) |> List.map Ty.neg |> Ty.conj in
        res := (Ty.cap ps ns)::(!res) ; false
    )
  in
  let remaining = Abstract.transform aux factor in
  match !res with
  | [] -> [ Subst.identity ]
  | res -> (remaining::res) |> List.map (fun ty -> Subst.construct [v, ty])
let abstract_factors cache sols (v,t) =
  let ss = abstract_factors cache v t in
  sols |> List.map (fun sol -> List.map (fun s -> Subst.compose s sol) ss) |> List.flatten
let abstract_factors cache tvars sol =
  (* Note: this simplification does nothing if parameters are fully annotated *)
  if !Config.no_abstract_inter then
    List.fold_left (abstract_factors cache) [sol]
      (Subst.restrict sol tvars |> Subst.destruct)
  else
    [sol]

let minimize_new_tvars tvars sol (v,t) =
  let mono = TVarSet.union_many [TVar.user_vars () ;  TVarSet.construct [v] ; tvars] in
  let ss = tallying mono [(TVar.typ v, t) ; (t, TVar.typ v)] in
  let aux sol s =
    let aux sol (v,t) =
      let r = substitute_by_similar_var v t in
      Subst.compose r sol
    in
    List.fold_left aux sol (Subst.destruct s)
  in
  List.fold_left aux sol ss
let minimize_new_tvars tvars sol =
  List.fold_left (minimize_new_tvars tvars) sol (Subst.destruct sol)

let tallying_simpl cache env res cs =
  let tvars = Env.tvars env in
  let leq_sol (_,r1) (_,r2) = Ty.leq r1 r2 in
  (* Format.printf "Tallying:@." ;
  cs |> List.iter (fun (a,b) -> Format.printf "%a <= %a@." Ty.pp a Ty.pp b) ; *)
  (* Format.printf "with tvars=%a@." (Utils.pp_list TVar.pp)
    (TVarSet.destruct tvars) ; *)
  (* Format.printf "with env=%a@." Env.pp env ; *)
  tallying_with_prio (TVar.user_vars ()) (tvars |> TVarSet.destruct) cs
  |> List.map (abstract_factors cache tvars) |> List.flatten
  |> List.map (minimize_new_tvars tvars)
  |> List.map (fun s -> s, Subst.apply s res)
  (* Simplify result if it does not impact the domains *)
  |> List.map (fun (s,r) ->
    let mono = Subst.restrict s tvars |> Subst.vars in
    let mono = TVarSet.union_many [mono ; tvars ; TVar.user_vars ()] in
    let clean = clean_subst ~pos:Ty.empty ~neg:Ty.any mono r in
    (Subst.compose clean s, Subst.apply clean r)
  )
  |> tsort leq_sol
  (* |> List.map (fun (s,r) -> Format.printf "%a@.%a@." Subst.pp s Ty.pp r ; s,r) *)

(* Reconstruction algorithm *)

type ('a,'b) result_seq =
| AllOk of 'a list * GTy.t list
| OneFail
| OneSubst of (Subst.t * Ty.t) list * 'b list * 'b list * (Eid.t * REnv.t)

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

let rec infer cache env renvs annot (id, e) =
  let open IAnnot in
  let log msg descr =
    let log = { eid=id ; title=msg ; descr } in
    cache.logs := log::!(cache.logs)
  in
  let retry_with a = infer cache env renvs a (id, e) in
  let to_i =
    (function Annot.BSkip -> IAnnot.BSkip | Annot.BType a -> IAnnot.BType (A a)) in
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
  | Constructor (_, es), Infer -> retry_with (AConstruct (List.map (fun _ -> Infer) es))
  | Constructor (c, es), AConstruct annots ->
    begin match infer_seq' cache env renvs (List.combine annots es) with
    | OneFail -> Fail
    | OneSubst (ss, a, a',r) -> Subst (ss,AConstruct a,AConstruct a',r)
    | AllOk (annots,tys) ->
      let doms = Checker.domains_of_construct c in
      let tys = List.map GTy.lb tys in
      let ss = tallying_simpl cache env (Checker.construct c tys) (List.combine tys doms) in
      log "untypeable constructor" (fun fmt ->
        Format.fprintf fmt "expected: %a\ngiven: %a"
          (Utils.pp_seq Ty.pp " ; ") doms
          (Utils.pp_seq Ty.pp " ; ") tys
        ) ;
      Subst (ss, nc (Annot.AConstruct annots), Untyp, empty_cov)
    end
  | Lambda (d,_,_), Infer -> retry_with (ALambda (d, Infer))
  | Lambda (_,v,e'), ALambda (ty, annot') ->
    let env' = Env.add v (TyScheme.mk_mono ty) env in
    begin match infer' { cache with dom=Domain.empty } env' renvs annot' e' with
    | Ok (annot', _) -> retry_with (nc (Annot.ALambda (ty, annot')))
    | Subst (ss,a,a',(eid,r)) ->
      Subst (ss,ALambda (ty, a),ALambda (ty, a'),(eid,REnv.add v (GTy.lb ty) r))
    | Fail -> Fail
    end
  | LambdaRec lst, Infer ->
    retry_with (ALambdaRec (List.map (fun (ty,_,_) -> ty, Infer) lst))
  | LambdaRec lst, ALambdaRec anns ->
    let lst = List.combine lst anns in
    let env' = lst |> List.fold_left
      (fun env ((_,v,_),(ty,_)) -> Env.add v (TyScheme.mk_mono ty) env) env in
    let tys = List.map fst anns in
    let aes = List.map (fun ((_,_,e),(_,a)) -> a,e) lst in
    begin match infer_seq' { cache with dom=Domain.empty } env' renvs aes with
    | OneFail -> Fail
    | OneSubst (ss, a, a',(eid,r)) ->
      let r = lst |> List.fold_left
        (fun r ((_,v,_),(ty,_)) -> REnv.add v (GTy.lb ty) r) r in
      Subst (ss,ALambdaRec (List.combine tys a),ALambdaRec (List.combine tys a'),(eid,r))
    | AllOk (annots,tys') ->
      let tys' = List.map GTy.lb tys' in
      let cs = List.combine tys' (List.map GTy.lb tys) in
      let ss = tallying_simpl cache env (Tuple.mk tys') cs in
      let ok_ann = nc (Annot.ALambdaRec (List.combine tys annots)) in
      log "untypeable recursive function" (fun fmt ->
        Format.fprintf fmt "cannot unify the body with self"
        ) ;
      Subst (ss, ok_ann, Untyp, empty_cov)
    end
  | Ite _, Infer -> retry_with (AIte (Infer, BInfer, BInfer))
  | Ite (e0,tau,e1,e2), AIte (a0,a1,a2) ->
    begin match infer' cache env renvs a0 e0 with
    | Fail -> Fail
    | Subst (ss,a,a',r) -> Subst (ss,AIte (a,a1,a2),AIte (a',a1,a2),r)
    | Ok (a0, s) ->
      begin match infer_b' cache env renvs a1 e1 s tau with
      | Fail -> Fail
      | Subst (ss, a1, a1',r) ->
        Subst (ss, AIte(A a0,a1,a2), AIte(A a0,a1',a2),r)
      | Ok (a1,_) ->
        begin match infer_b' cache env renvs a2 e2 s (Ty.neg tau) with
        | Fail -> Fail
        | Subst (ss, a2, a2',r) ->
          Subst (ss, AIte(A a0,to_i a1,a2), AIte(A a0,to_i a1,a2'),r)
        | Ok (a2,_) -> retry_with (nc (Annot.AIte(a0,a1,a2)))
        end  
      end
    end
  | ControlFlow _, Infer -> retry_with (ACf (Infer, BInfer, BInfer))
  | ControlFlow (_,e0,tau,e1,e2), ACf (a0,a1,a2) ->
    begin match infer' cache env renvs a0 e0 with
    | Fail -> Fail
    | Subst (ss,a,a',r) -> Subst (ss,ACf (a,a1,a2),ACf (a',a1,a2),r)
    | Ok (a0, s) ->
      begin match infer_cf_b' cache env renvs a1 e1 s tau with
      | Fail -> Fail
      | Subst (ss, a1, a1',r) ->
        Subst (ss, ACf(A a0,a1,a2), ACf(A a0,a1',a2),r)
      | Ok (a1,_) ->
        begin match infer_cf_b' cache env renvs a2 e2 s (Ty.neg tau) with
        | Fail -> Fail
        | Subst (ss, a2, a2',r) ->
          Subst (ss, ACf(A a0,to_i a1,a2), ACf(A a0,to_i a1,a2'),r)
        | Ok (a2,_) -> retry_with (nc (Annot.ACf(a0,a1,a2)))
        end  
      end
    end
  | App _, Infer -> retry_with (AApp (Infer, Infer))
  | App (e1, e2), AApp (a1,a2) ->
    begin match infer_seq' cache env renvs [(a1,e1);(a2,e2)] with
    | OneFail -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2'],r) ->
      Subst (ss,AApp(a1,a2),AApp(a1',a2'),r)
    | AllOk ([a1;a2],[t1;t2]) ->
      let tv = TVCache.get cache.tvcache id TVCache.res_tvar in
      let t1, t2 = GTy.lb t1, GTy.lb t2 in
      let arrow = Arrow.mk t2 (TVar.typ tv) in
      let ss = tallying_simpl cache env (TVar.typ tv) [(t1, arrow)] in
      log "untypeable application" (fun fmt ->
        Format.fprintf fmt "function: %a\nargument: %a" Ty.pp t1 Ty.pp t2
        ) ;
      Subst (ss, nc (Annot.AApp(a1,a2)), Untyp, empty_cov)
    | _ -> assert false
    end
  | Projection _, Infer -> retry_with (AProj Infer)
  | Projection (p,e'), AProj annot' ->
    begin match infer' cache env renvs annot' e' with
    | Ok (annot', s) ->
      let tv = TVCache.get cache.tvcache id TVCache.res_tvar in
      let ty = Checker.domain_of_proj p (TVar.typ tv) in
      let s = GTy.lb s in
      let ss = tallying_simpl cache env (TVar.typ tv) [(s, ty)] in
      log "untypeable projection" (fun fmt ->
        Format.fprintf fmt "argument: %a" Ty.pp s
        ) ;
      Subst (ss, nc (Annot.AProj annot'), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,AProj a,AProj a',r)
    | Fail -> Fail
    end
  | Let (suggs,v,_,_), Infer ->
    let tys = Refinement.Partitioner.partition_for renvs v suggs in
    retry_with (ALet (Infer, List.map (fun ty -> (ty, Infer)) tys))
  | Let (_,v,e1,e2), ALet(annot1,parts) ->
    begin match infer' cache env renvs annot1 e1 with
    | Fail -> Fail
    | Subst (ss,a,a',r) -> Subst (ss,ALet (a,parts),ALet (a',parts),r)
    | Ok (annot1, s) ->
      let tvs, s = Checker.generalize ~e:e1 env s |> TyScheme.get in
      let parts = parts |> List.filter (fun (t,_) -> Ty.disjoint (GTy.ub s) t |> not) in
      begin match infer_part_seq' cache env renvs e2 v (tvs,s) parts with
      | OneFail -> Fail
      | OneSubst (ss,p,p',r) -> Subst (ss,ALet(A annot1,p),ALet(A annot1,p'),r)
      | AllOk (p,_) -> retry_with (nc (Annot.ALet (annot1, p)))
      end
    end
  | TypeCast _, Infer -> retry_with (ACast Infer)
  | TypeCoerce (_,t, _), Infer -> retry_with (ACoerce (t,Infer))
  | TypeCast (e', t), ACast annot' ->
    begin match infer' cache env renvs annot' e' with
    | Ok (annot', s) ->
      let s = GTy.lb s in
      let ss = tallying_simpl cache env s [(s,t)] in
      log "untypeable constraint" (fun fmt ->
        Format.fprintf fmt "expected: %a\ngiven: %a" Ty.pp t Ty.pp s
        ) ;
      Subst (ss, nc (Annot.ACast(annot')), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,ACast a,ACast a',r)
    | Fail -> Fail
    end
  | TypeCoerce (e', _, c), ACoerce (t,annot') ->
    begin match infer' cache env renvs annot' e' with
    | Ok (annot', s) ->
      let lbc, ubc = (GTy.lb s, GTy.lb t), (GTy.ub s, GTy.ub t) in
      let cs = match c with
        | Check -> [lbc;ubc] | CheckStatic -> [lbc] | NoCheck -> [] in
      let ss = tallying_simpl cache env (GTy.lb t) cs in
      log "untypeable coercion" (fun fmt ->
        if c = Check then
          Format.fprintf fmt "expected: %a\ngiven: %a" GTy.pp t GTy.pp s
        else if c = CheckStatic then
          Format.fprintf fmt "expected: %a\ngiven: %a" Ty.pp (GTy.lb t) Ty.pp (GTy.lb s)
        ) ;
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
          begin match infer' {cache with dom} env renvs ann (id,e) with
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
and infer' cache env renvs annot e =
  let mono = TVarSet.union (Env.tvars env) (TVar.user_vars ()) in
  let subst_disjoint s =
    TVarSet.inter (Subst.dom s) mono |> TVarSet.is_empty
  in
  match infer cache env renvs annot e with
  | Ok (a, ty) -> Ok (a, ty)
  | Fail -> Fail
  | Subst (ss, a1, a2, (eid,r)) when ss |> List.map fst |> List.for_all subst_disjoint ->
    let default =
      (* Don't add default branch if already covered (also important for error msg) *)
      if ss |> List.exists (fun (s,_) -> Subst.is_identity s)
      then [] else [{ IAnnot.coverage=(Some (None, r)) ; ann=a2 }]
    in
    let branches = ss |> List.map (fun (s,ty) ->
      let ann = IAnnot.substitute s a1 in
      let coverage = (Some (eid, ty), REnv.substitute s r) in
      { IAnnot.coverage=(Some coverage) ; ann }
      ) in
    let annot = IAnnot.AInter (branches@default) in
    infer' cache env renvs annot e
  | Subst (ss, a1, a2, r) -> Subst (ss, a1, a2, r)
and infer_b' cache env renvs bannot e s tau =
  let empty_cov = (fst e, REnv.empty) in
  match bannot with
  | IAnnot.BInfer ->
    let ss = tallying_simpl cache env Ty.empty [(GTy.ub s,Ty.neg tau)] in
    Subst (ss, IAnnot.BSkip, IAnnot.BType Infer, empty_cov)
  | IAnnot.BSkip -> Ok (Annot.BSkip, GTy.empty)
  | IAnnot.BType annot ->
    begin match infer' cache env renvs annot e with
    | Ok (a, ty) -> Ok (Annot.BType a, ty)
    | Subst (ss,a1,a2,r) -> Subst (ss,IAnnot.BType a1,IAnnot.BType a2,r)
    | Fail -> Fail
    end
and infer_cf_b' cache env renvs bannot e s tau =
  let retry_with bannot = infer_cf_b' cache env renvs bannot e s tau in
  match bannot with
  | IAnnot.BInfer ->
    if Ty.leq (GTy.ub s) (Ty.neg tau)
    then retry_with (IAnnot.BSkip)
    else retry_with (IAnnot.BType Infer)
  | IAnnot.BSkip -> Ok (Annot.BSkip, GTy.empty)
  | IAnnot.BType annot ->
    begin match infer' cache env renvs annot e with
    | Ok (a, ty) (* when Ty.leq ty Ty.unit *) -> Ok (Annot.BType a, ty)
    (* | Ok _ -> Fail *)
    | Subst (ss,a1,a2,r) -> Subst (ss,IAnnot.BType a1,IAnnot.BType a2,r)
    | Fail -> Fail
    end
and infer_part' cache env renvs e v (tvs, s) (si,annot) =
  let t = TyScheme.mk tvs (GTy.cap s (GTy.mk si)) in
  let env = Env.add v t env in
  let renvs = Refinement.Partitioner.filter_compatible renvs v si in
  match infer' cache env renvs annot e with
  | Fail -> Fail
  | Subst (ss,a,a',r) -> Subst (ss,(si,a),(si,a'),r)
  | Ok (a,ty) -> Ok ((si,a),ty)
and infer_seq' cache env renvs lst = seq (infer' cache env renvs) (fun a -> A a) lst
and infer_part_seq' cache env renvs e v s lst =
  seq (fun a () -> infer_part' cache env renvs e v s a)
    (fun (si,annot) -> (si, A annot))
    (lst |> List.map (fun a -> (a,())))

let infer env renvs e =
  let renvs = Refinement.Partitioner.from_renvset renvs in
  let cache = { dom = Domain.empty ; tvcache = TVCache.empty () ; logs = ref [] } in
  match infer' cache env renvs IAnnot.Infer e with
  | Fail ->
    begin match !(cache.logs) with
    | [] ->
      let err = { Checker.eid=Eid.dummy ;
        title="annotation reconstruction failed" ; descr=None } in
      raise (Checker.Untypeable err)
    | log::_ ->
      let err = { Checker.eid=log.eid ; title=log.title ;
        descr=(Some (Format.asprintf "%a" (fun fmt () -> log.descr fmt) ())) } in
      raise (Checker.Untypeable err)
    end
  | Subst _ -> failwith "Top-level environment should not contain an unresolved type variable."
  | Ok (a,_) -> a
