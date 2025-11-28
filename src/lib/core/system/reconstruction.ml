open Mlsem_common
open Annot
open Mlsem_types
open TVOp
open Ast
open Mlsem_utils

(* ===== Initial Annot ===== *)

let rec initial renv (_, e) =
  let new_renaming () =
    let s = ref Subst.identity in
    fun dom ->
      let dom' = Subst.domain !s in
      let dom = MVarSet.diff dom dom' in
      let s' = TVOp.refresh ~kind:KInfer dom in
      s := Subst.combine !s s' ; !s
  in
  let new_result () =
    TVar.mk KInfer None |> TVar.typ
  in
  let open IAnnot in
  match e with
  | Value ty -> A (AValue ty |> Annot.nc)
  | Var _ -> AVar (new_renaming ())
  | Constructor (_,es) -> AConstruct (List.map (initial renv) es)
  | Lambda (dom, _, e) -> ALambda (dom, initial renv e)
  | LambdaRec lst -> ALambdaRec (lst |> List.map (fun (dom, _, e) -> dom, initial renv e))
  | Ite (e, tau, e1, e2) ->
    AIte (initial renv e, tau, BType (false, initial renv e1), BType (false, initial renv e2))
  | App (e1, e2) -> AApp (initial renv e1, initial renv e2, new_result ())
  | Operation (_, e) -> AOp (new_renaming (), initial renv e, new_result ())
  | Projection (_, e) -> AProj (initial renv e, new_result ())
  | TypeCast (e, ty, _) -> ACast (ty, initial renv e)
  | TypeCoerce (e, ty, _) -> ACoerce (ty, initial renv e)
  | Alt (e1, e2) -> AAlt (Some (initial renv e1), Some (initial renv e2))
  | Let (suggs, v, e1, e2) ->
    let a1 = initial renv e1 in
    let tys = Refinement.Partitioner.partition_for renv v suggs in
    let parts = tys |> List.map (fun ty ->
        let renv = Refinement.Partitioner.filter_compatible renv v ty in
        (ty, initial renv e2)
      ) in
    ALet (a1, parts)

let initial renv e =
  let renv = Refinement.Partitioner.from_renvset renv in
  initial renv e

(* ===== Annotation Reconstruction ===== *)

type ('a,'b) result =
| Ok of 'a * GTy.t
| Fail
| Subst of (Subst.t * Ty.t) list * 'b * 'b * (Eid.t * REnv.t)

type log = { eid: Eid.t ; title: string ; descr: Format.formatter -> unit }
type cache = { dom : Domain.t ; logs : log list ref }

(* Auxiliary *)

let tsort leq lst =
  let rec add_elt lst ne =
    match lst with
    | [] -> [ne]
    | e::lst when leq ne e -> ne::e::lst
    | e::lst -> e::(add_elt lst ne)
  in
  List.fold_left add_elt [] (List.rev lst)

let abstract_factors v ty =
  let (factor, _) = factorize (TVarSet.singleton v, TVarSet.empty) ty in
  let res = ref [] in
  let aux (abs, dnf) =
    dnf |> List.filter (fun (ps, ns) ->
      if ps = [] then true
      else
        let ps = ps |> List.map (Abstract.mk abs) |> Ty.conj in
        let ns = ns |> List.map (Abstract.mk abs) |> List.map Ty.neg |> Ty.conj in
        res := (Ty.cap ps ns)::(!res) ; false
    )
  in
  let remaining = Abstract.transform aux factor in
  match !res with
  | [] -> [ Subst.identity ]
  | res -> (remaining::res) |> List.map (fun ty -> Subst.singleton1 v ty)
let abstract_factors sols (v,t) =
  let ss = abstract_factors v t in
  sols |> List.concat_map (fun sol -> List.map (fun s -> Subst.compose s sol) ss)
let abstract_factors tvars sol =
  (* Note: this simplification does nothing if parameters are fully annotated *)
  if !Config.no_abstract_inter then
    List.fold_left abstract_factors [sol]
      (Subst.restrict tvars sol |> Subst.bindings1)
  else
    [sol]

let substitute_similar_vars1 mono v t =
  let vs = MVarSet.diff (top_vars t) (MVarSet.add1 v mono) in
  let nt = vars_with_polarity1 t |> List.filter_map (fun (v', k) ->
    if MVarSet.mem1 v' vs then
    match k with
    | `Pos -> Some (v', TVar.typ v)
    | `Neg -> Some (v', TVar.typ v |> Ty.neg)
    | `Both -> (* Cases like Bool & 'a \ 'b  |  Int & 'a & 'b *) None
    else None
    )
  in
  Subst.of_list1 nt
let substitute_similar_vars2 mono v row =
  let vs = RVarSet.diff
    (Row.row_vars_toplevel row) (MVarSet.proj2 mono |> RVarSet.add v) in
  let tail = Record.mk' (Row.tail row) [] in
  let nrow = vars_with_polarity2 tail |> List.filter_map (fun (v', k) ->
    if RVarSet.mem v' vs then
    match k with
    | `Pos -> Some (v', RVar.row v)
    | `Neg -> Some (v', (RVar.fty v |> FTy.neg) |> Row.all_fields)
    | `Both -> None
    else None
    )
  in
  Subst.of_list2 nrow
let minimize_new_tvars mono sol =
  let minimize_binding1 sol (v,t) =
    let r = substitute_similar_vars1 mono v t in
    Subst.compose r sol
  in
  let minimize_binding2 sol (v,row) =
    let r = substitute_similar_vars2 mono v row in
    Subst.compose r sol
  in
  let res = List.fold_left minimize_binding1 sol (Subst.bindings1 sol) in
  List.fold_left minimize_binding2 res (Subst.bindings2 sol)

let tallying_simpl mono tvars res cs =
  let ntvars s = MVarSet.union tvars (Subst.restrict tvars s |> Subst.intro) in
  let is_better (s1,r1) (s2,r2) =
    let mono2 = List.fold_left MVarSet.union MVarSet.empty
      [ mono ; ntvars s2 ; TVOp.vars r2 ] in
    TVOp.decompose mono (Subst.restrict tvars s1) (Subst.restrict tvars s2)
    |> List.exists (fun s' -> TVOp.tallying mono2 [(Subst.apply s' r1, r2)] <> [])
  in
  let not_redundant s ss =
    ss |> List.for_all (fun s' -> is_better s' s |> not)
  in
  (* Format.printf "Tallying:@." ;
  cs |> List.iter (fun (a,b) -> Format.printf "%a <= %a@." Ty.pp a Ty.pp b) ; *)
  (* Format.printf "with tvars=%a@." (Utils.pp_list TVar.pp)
    (TVarSet.destruct tvars) ; *)
  (* Format.printf "with env=%a@." Env.pp env ; *)
  tallying_fields mono cs
  |> List.concat_map (abstract_factors tvars)
  |> List.map (minimize_new_tvars (MVarSet.union mono tvars))
  |> List.map (fun s -> s, Subst.apply s res)
  (* Simplify result if it does not impact the domains *)
  |> List.map (fun (s,r) ->
    let mono = MVarSet.union mono (ntvars s) in
    let clean = clean_subst
      ~pos1:Ty.empty ~neg1:Ty.any ~pos2:Row.empty ~neg2:Row.any mono r in
    (Subst.compose clean s, Subst.apply clean r)
  )
  |> Utils.filter_among_others not_redundant
  |> tsort (fun (_,r1) (_,r2) -> Ty.leq r1 r2)
  (* |> List.map (fun (s,r) -> Format.printf "%a@.%a@." Subst.pp s Ty.pp r ; s,r) *)

let tallying_simpl env res cs =
  let mono = MVarSet.of_set (TVar.all_vars KNoInfer) (RVar.all_vars KNoInfer) in
  let tvars = Env.tvars env in
  let fc = TVOp.get_field_ctx (MVarSet.proj2 mono) (cs |> List.concat_map (fun (a,b) -> [a;b])) in
  let tvars = MVarSet.elements2 tvars |> List.fold_left
    (fun acc rv -> MVarSet.union acc (TVOp.fvars_associated_with fc rv |> MVarSet.of_set2)) tvars in
  cs |> List.map (fun (a,b) -> (TVOp.decorrelate_fields fc a, TVOp.decorrelate_fields fc b))
     |> tallying_simpl mono tvars (TVOp.decorrelate_fields fc res)
     |> List.map (fun (s,r) -> TVOp.recombine_fields' fc s, TVOp.recombine_fields fc r)

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

let rec refine cache env annot (id, e) =
  let open IAnnot in
  let log msg descr =
    let log = { eid=id ; title=msg ; descr } in
    cache.logs := log::!(cache.logs)
  in
  let retry_with a = refine cache env a (id, e) in
  let to_i =
    (function Annot.BSkip -> IAnnot.BSkip | Annot.BType a -> IAnnot.BType (true, A a)) in
  let empty_cov = (id, REnv.empty) in
  let app res t1 t2 =
    let t1, t2 = GTy.lb t1, GTy.lb t2 in
    let arrow = Arrow.mk t2 res in
    let ss = tallying_simpl env res [(t1, arrow)] in
    let ss = if !Config.infer_overload || Ty.is_empty t2 then ss else
      ss |> List.filter (fun (s, _) -> Subst.apply s t2 |> Ty.non_empty)
    in
    log "untypeable application" (fun fmt ->
      Format.fprintf fmt "function: %a\nargument: %a" Ty.pp t1 Ty.pp t2
      ) ;
    ss
  in
  match e, annot with
  | _, A a -> Ok (a, Checker.typeof env a (id, e))
  | _, Untyp -> Fail
  | Var v, AVar f when Env.mem v env ->
    let tvs, _ = Env.find v env |> TyScheme.get in
    let s = f tvs in
    retry_with (nc (Annot.AVar s))
  | Var v, AVar _ ->
    log "unbound variable" (fun fmt -> Format.fprintf fmt "name: %a" Variable.pp v) ;
    Fail
  | Constructor (c, es), AConstruct annots ->
    begin match refine_seq' cache env (List.combine annots es) with
    | OneFail -> Fail
    | OneSubst (ss, a, a',r) -> Subst (ss,AConstruct a,AConstruct a',r)
    | AllOk (annots,tys) ->
      let doms = Checker.domains_of_construct c Ty.any in
      let tys = List.map GTy.lb tys in
      let ss =
        doms |> List.concat_map (fun doms ->
        tallying_simpl env (Checker.construct c tys) (List.combine tys doms)
      ) in
      log "untypeable constructor" (fun fmt ->
        Format.fprintf fmt "expected: %a\ngiven: %a"
          (Utils.pp_seq (Utils.pp_seq Ty.pp " ; ") " ;; ") doms
          (Utils.pp_seq Ty.pp " ; ") tys
        ) ;
      Subst (ss, nc (Annot.AConstruct annots), Untyp, empty_cov)
    end
  | Lambda (_,v,e'), ALambda (ty, annot') ->
    let env' = Env.add v (TyScheme.mk_mono ty) env in
    begin match refine' { cache with dom=Domain.empty } env' annot' e' with
    | Ok (annot', _) -> retry_with (nc (Annot.ALambda (ty, annot')))
    | Subst (ss,a,a',(eid,r)) ->
      Subst (ss,ALambda (ty, a),ALambda (ty, a'),(eid,REnv.add v (GTy.lb ty) r))
    | Fail -> Fail
    end
  | LambdaRec lst, ALambdaRec anns ->
    let lst = List.combine lst anns in
    let env' = lst |> List.fold_left
      (fun env ((_,v,_),(ty,_)) -> Env.add v (TyScheme.mk_mono ty) env) env in
    let tys = List.map fst anns in
    let aes = List.map (fun ((_,_,e),(_,a)) -> a,e) lst in
    begin match refine_seq' { cache with dom=Domain.empty } env' aes with
    | OneFail -> Fail
    | OneSubst (ss, a, a',(eid,r)) ->
      let r = lst |> List.fold_left
        (fun r ((_,v,_),(ty,_)) -> REnv.add v (GTy.lb ty) r) r in
      Subst (ss,ALambdaRec (List.combine tys a),ALambdaRec (List.combine tys a'),(eid,r))
    | AllOk (annots,tys') ->
      let tys' = List.map GTy.lb tys' in
      let cs = List.combine tys' (List.map GTy.lb tys) in
      let ss = tallying_simpl env (Tuple.mk tys') cs in
      let ok_ann = nc (Annot.ALambdaRec (List.combine tys annots)) in
      log "untypeable recursive function" (fun fmt ->
        Format.fprintf fmt "cannot unify the body with self"
        ) ;
      Subst (ss, ok_ann, Untyp, empty_cov)
    end
  | Ite (e0,_,e1,e2), AIte (a0,tau,a1,a2) ->
    begin match refine' cache env a0 e0 with
    | Fail -> Fail
    | Subst (ss,a,a',r) -> Subst (ss,AIte (a,tau,a1,a2),AIte (a',tau,a1,a2),r)
    | Ok (a0, s) ->
      begin match refine_b' cache env a1 e1 s tau with
      | Fail -> Fail
      | Subst (ss, a1, a1',r) ->
        Subst (ss, AIte(A a0,tau,a1,a2), AIte(A a0,tau,a1',a2),r)
      | Ok (a1,_) ->
        begin match refine_b' cache env a2 e2 s (GTy.neg tau) with
        | Fail -> Fail
        | Subst (ss, a2, a2',r) ->
          Subst (ss, AIte(A a0,tau,to_i a1,a2), AIte(A a0,tau,to_i a1,a2'),r)
        | Ok (a2,_) -> retry_with (nc (Annot.AIte(a0,tau,a1,a2)))
        end  
      end
    end
  | Alt _, AAlt (None, None) -> Fail
  | Alt _, AAlt (Some (A a1), None) -> retry_with (nc (Annot.AAlt(Some a1,None)))
  | Alt _, AAlt (None, Some (A a2)) -> retry_with (nc (Annot.AAlt(None,Some a2)))
  | Alt _, AAlt (Some (A a1), Some (A a2)) -> retry_with (nc (Annot.AAlt(Some a1,Some a2)))
  | Alt (_, e2), AAlt ((None | Some (A _) as a1), Some a2) ->
    begin match refine' cache env a2 e2 with
    | Ok (a2, _) -> retry_with (AAlt (a1, Some (A a2)))
    | Subst (ss,a2,a2',r) -> Subst (ss,AAlt (a1,Some a2),AAlt (a1,Some a2'),r)
    | Fail -> retry_with (AAlt (a1, None))
    end
  | Alt (e1, _), AAlt (Some a1, a2) ->
    begin match refine' cache env a1 e1 with
    | Ok (a1, _) -> retry_with (AAlt (Some (A a1), a2))
    | Subst (ss,a1,a1',r) -> Subst (ss,AAlt (Some a1,a2),AAlt (Some a1',a2),r)
    | Fail -> retry_with (AAlt (None, a2))
    end
  | App (e1, e2), AApp (a1,a2,res) ->
    begin match refine_seq' cache env [(a1,e1);(a2,e2)] with
    | OneFail -> Fail
    | OneSubst (ss, [a1;a2], [a1';a2'],r) ->
      Subst (ss,AApp(a1,a2,res),AApp(a1',a2',res),r)
    | AllOk ([a1;a2],[t1;t2]) ->
      let ss = app res t1 t2 in
      Subst (ss, nc (Annot.AApp(a1,a2,res)), Untyp, empty_cov)
    | _ -> assert false
    end
  | Operation (o,e'), AOp (f,annot',res) ->
    begin match refine' cache env annot' e' with
    | Ok (annot', t') ->
      let tvs, t = Checker.fun_of_operation o |> TyScheme.get in
      let s = f tvs in
      let t = GTy.substitute s t in
      let ss = app res t t' in
      Subst (ss, nc (Annot.AOp(s,annot',res)), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,AOp (f,a,res),AOp (f,a',res),r)
    | Fail -> Fail
    end
  | Projection (p,e'), AProj (annot',res) ->
    begin match refine' cache env annot' e' with
    | Ok (annot', s) ->
      let ty = Checker.domain_of_proj p res in
      let s = GTy.lb s in
      let ss = tallying_simpl env res [(s, ty)] in
      log "untypeable projection" (fun fmt ->
        Format.fprintf fmt "argument: %a" Ty.pp s
        ) ;
      Subst (ss, nc (Annot.AProj annot'), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,AProj (a,res),AProj (a',res),r)
    | Fail -> Fail
    end
  | Let (_,v,e1,e2), ALet(annot1,parts) ->
    begin match refine' cache env annot1 e1 with
    | Fail -> Fail
    | Subst (ss,a,a',r) -> Subst (ss,ALet (a,parts),ALet (a',parts),r)
    | Ok (annot1, s) ->
      let tvs, s = Checker.generalize ~e:e1 env s |> TyScheme.get in
      let parts = parts |> List.filter (fun (t,_) -> Ty.disjoint (GTy.ub s) t |> not) in
      begin match refine_part_seq' cache env e2 v (tvs,s) parts with
      | OneFail -> Fail
      | OneSubst (ss,p,p',r) -> Subst (ss,ALet(A annot1,p),ALet(A annot1,p'),r)
      | AllOk (p,_) -> retry_with (nc (Annot.ALet (annot1, p)))
      end
    end
  | TypeCast (e', _, c), ACast (t,annot') ->
    begin match refine' cache env annot' e' with
    | Ok (annot', s) ->
      let lbc, ubc = (GTy.lb s, GTy.lb t), (GTy.ub s, GTy.ub t) in
      let cs = match c with
        | Check -> [lbc;ubc] | CheckStatic -> [lbc] | NoCheck -> [] in
      let ss = tallying_simpl env (GTy.lb (GTy.cap t s)) cs in
      log "untypeable cast" (fun fmt ->
        if c = Check then
          Format.fprintf fmt "expected: %a\ngiven: %a" GTy.pp t GTy.pp s
        else if c = CheckStatic then
          Format.fprintf fmt "expected: %a\ngiven: %a" Ty.pp (GTy.lb t) Ty.pp (GTy.lb s)
        ) ;
      Subst (ss, nc (Annot.ACast(t,annot')), Untyp, empty_cov)
    | Subst (ss,a,a',r) -> Subst (ss,ACast (t,a),ACast (t,a'),r)
    | Fail -> Fail
    end
  | TypeCoerce (e', _, c), ACoerce (t,annot') ->
    begin match refine' cache env annot' e' with
    | Ok (annot', s) ->
      let lbc, ubc = (GTy.lb s, GTy.lb t), (GTy.ub s, GTy.ub t) in
      let cs = match c with
        | Check -> [lbc;ubc] | CheckStatic -> [lbc] | NoCheck -> [] in
      let ss = tallying_simpl env (GTy.lb t) cs in
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
          begin match refine' {cache with dom} env ann (id,e) with
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
and refine' cache env annot e =
  let tvars = Env.tvars env in
  let subst_disjoint s =
    MVarSet.inter (Subst.domain s) tvars |> MVarSet.is_empty
  in
  match refine cache env annot e with
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
    refine' cache env annot e
  | Subst (ss, a1, a2, r) -> Subst (ss, a1, a2, r)
and refine_b' cache env bannot e s tau =
  let retry_with bannot = refine_b' cache env bannot e s tau in
  let empty_cov = (fst e, REnv.empty) in
  match bannot with
  | IAnnot.BType (false, annot) when !Config.infer_overload ->
    let ss = tallying_simpl env Ty.empty [(GTy.ub s, GTy.ub (GTy.neg tau))] in
    Subst (ss, IAnnot.BSkip, IAnnot.BType (true, annot), empty_cov)
  | IAnnot.BType (false, _) when GTy.disjoint s tau -> retry_with (IAnnot.BSkip)
  | IAnnot.BType (false, annot) -> retry_with (IAnnot.BType (true, annot))
  | IAnnot.BSkip -> Ok (Annot.BSkip, GTy.empty)
  | IAnnot.BType (true, annot) ->
    begin match refine' cache env annot e with
    | Ok (a, ty) -> Ok (Annot.BType a, ty)
    | Subst (ss,a1,a2,r) -> Subst (ss,IAnnot.BType (true, a1),IAnnot.BType (true, a2),r)
    | Fail -> Fail
    end
and refine_part' cache env e v (tvs, s) (si,annot) =
  let t = TyScheme.mk tvs (GTy.cap s (GTy.mk si)) in
  let env = Env.add v t env in
  match refine' cache env annot e with
  | Fail -> Fail
  | Subst (ss,a,a',r) -> Subst (ss,(si,a),(si,a'),r)
  | Ok (a,ty) -> Ok ((si,a),ty)
and refine_seq' cache env lst = seq (refine' cache env) (fun a -> A a) lst
and refine_part_seq' cache env e v s lst =
  seq (fun a () -> refine_part' cache env e v s a)
    (fun (si,annot) -> (si, A annot))
    (lst |> List.map (fun a -> (a,())))

let refine env iannot e =
  let cache = { dom = Domain.empty ; logs = ref [] } in
  match refine' cache env iannot e with
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

let infer env renv e = refine env (initial renv e) e 