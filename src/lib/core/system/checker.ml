open Mlsem_common
open Mlsem_types
open Annot
open Ast

(* Projections and constructors *)

let domain_of_proj p ty =
  match p with
  | PiField label -> Record.mk_open [label, (ty, false)]
  | PiFieldOpt label -> Record.mk_open [label, (ty, true)]
  | Pi(n,i) ->
    if i >= n then Ty.empty
    else Tuple.mk (List.init n (fun k -> if k=i then ty else Ty.any))
  | Hd ->
    Lst.cons ty Lst.any
  | Tl ->
    Lst.cons Ty.any ty
  | PiTag tag ->
    Tag.mk tag ty
  | PCustom r -> r.pdom ty

let proj p ty =
  match p with
  | PiField label | PiFieldOpt label -> Record.proj ty label
  | Pi (n,i) -> Tuple.proj n i ty
  | Hd -> Lst.proj ty |> fst
  | Tl -> Lst.proj ty |> snd
  | PiTag tag -> Tag.proj tag ty
  | PCustom r -> r.proj ty

let domains_of_construct (c:Ast.constructor) ty =
  match c with
  | Tuple n ->
    Tuple.dnf n ty
    |> List.filter (fun b -> Ty.leq (Tuple.mk b) ty)
  | Join n | Meet n -> [List.init n (fun _ -> ty)]
  | Ternary _ -> [ [ Ty.any ; ty ; ty ] ]
  | Ignore ty' when Ty.leq ty' ty -> [ [Ty.any] ]
  | Ignore _ -> []
  | Cons ->
    Lst.dnf ty
    |> List.filter (fun (a,b) -> Ty.leq (Lst.cons a b) ty)
    |> List.map (fun (t1,t2) -> [t1;t2])
  | Rec (labels, opened) ->
    let mk = if opened then Record.mk_open else Record.mk_closed in
    Ty.cap ty (mk (List.map (fun lbl -> (lbl, (Ty.any,false))) labels))
    |> Record.dnf
    |> List.filter_map (fun (bindings,tail) ->
      let ty' = Record.mk tail bindings in
      if Ty.leq ty' ty
      then Some (List.map (fun lbl -> Record.proj ty' lbl) labels)
      else None
    )
  | Tag tag -> [ [ Tag.proj tag ty ] ]
  | Enum e when Ty.leq (Enum.typ e) ty -> [ [] ]
  | Enum _ -> [ ]
  | CCustom r -> r.cdom ty

let construct (c:Ast.constructor) tys =
  match c, tys with
  | Tuple n, tys when List.length tys = n -> Tuple.mk tys
  | Join n, tys when List.length tys = n -> Ty.disj tys
  | Meet n, tys when List.length tys = n -> Ty.conj tys
  | Ternary tau, [t;t1;t2] ->
    if Ty.leq t tau then t1
    else if Ty.leq t (Ty.neg tau) then t2
    else Ty.cup t1 t2
  | Ignore ty, [_] -> ty
  | Cons, [t1 ; t2] -> Lst.cons t1 t2
  | Rec (labels, opened), tys when List.length labels = List.length tys ->
    let mk = if opened then Record.mk_open else Record.mk_closed in
    let bindings = List.map2 (fun lbl ty -> (lbl, (ty,false))) labels tys in
    mk bindings
  | Tag tag, [t] -> Tag.mk tag t
  | Enum enum, [] -> Enum.typ enum
  | CCustom r, tys -> r.cons tys
  | _ -> raise (Invalid_argument "Invalid arity for constructor.")

let rv = RVar.mk KNoInfer None
let tv = TVar.mk KNoInfer None
let fun_of_operation o =
  match o with
  | RecUpd lbl ->
    let dom1, dom2 = Record.mk' (RVar.fty rv) [], TVar.typ tv in
    let codom = Record.mk' (RVar.fty rv) [(lbl, FTy.of_oty (TVar.typ tv, false))] in
    Arrow.mk (Tuple.mk [dom1;dom2]) codom |> GTy.mk |> TyScheme.mk_poly
  | RecDel lbl ->
    let dom = Record.mk' (RVar.fty rv) [] in
    let codom = Record.mk' (RVar.fty rv) [(lbl, FTy.of_oty (Ty.empty, true))] in
    Arrow.mk dom codom |> GTy.mk |> TyScheme.mk_poly
  | OCustom { ofun ; _ } -> ofun

(* Expressions *)

type error = { eid: Eid.t ; title: string ; descr: string option }
exception Untypeable of error

let untypeable id msg = raise (Untypeable { eid=id ; title=msg ; descr=None })

let proj_is_gen p =
  match p with
  | Pi _ | PiField _ | PiFieldOpt _ | Hd | Tl | PiTag _ -> true
  | PCustom c -> c.pgen
let constr_is_gen c =
  match c with
  | Tuple _ | Cons | Rec _ | Tag _ | Enum _
  | Join _ | Meet _  | Ternary _ -> true
  | Ignore _ -> false
  | CCustom c -> c.cgen
let op_is_gen o =
  match o with
  | RecUpd _ | RecDel _ -> true
  | OCustom c -> c.ogen
let rec is_gen (_,e) =
  match e with
  | Lambda _ | Value _ -> true
  | Var _ | App _ -> false
  | Constructor (c, es) -> constr_is_gen c && List.for_all is_gen es
  | Projection (p, e) -> proj_is_gen p && is_gen e
  | Operation (o, e) -> op_is_gen o && is_gen e
  | LambdaRec lst -> List.for_all (fun (_,_,e) -> is_gen e) lst
  | TypeCast (e, _, _) | TypeCoerce (e, _, _) -> is_gen e
  | Let (_, _, e1, e2) | Ite (_, _, e1, e2) | Alt (e1, e2) -> is_gen e1 && is_gen e2

let generalize ~e env s =
  if not (!Config.value_restriction) || is_gen e then
    TyScheme.mk_poly_except (Env.tvars env) s |> TyScheme.bot_instance
  else
    TyScheme.mk_mono s

let rec typeof' env annot (id,e) =
  let open Annot in
  let subst_ts s ts =
    let (tvs, ty) = TyScheme.get ts in
    if MVarSet.subset (Subst.domain s) tvs then GTy.substitute s ty
    else untypeable id ("Invalid substitution.")
  in
  let app t1 t2 =
    let check ty1 ty2 =
      Ty.leq ty1 Arrow.any && Ty.leq ty2 (Arrow.domain ty1)
    in
    begin match GTy.op2 check Arrow.apply t1 t2 with
    | Some ty -> ty
    | None -> untypeable id "Invalid application."
    end
  in
  match e, annot with
  | Value _, AValue ty -> ty
  | Var v, AVar s ->
    if Env.mem v env then Env.find v env |> subst_ts s
    else untypeable id ("Undefined variable "^(Variable.show v)^".")
  | Constructor (c, es), AConstruct annots when List.length es = List.length annots ->
    let doms = domains_of_construct c Ty.any in
    let check tys =
      doms |> List.exists (fun doms -> List.for_all2 Ty.leq tys doms)
    in
    let tys = List.map2 (fun e a -> typeof env a e) es annots in
    begin match GTy.opl check (construct c) tys with
    | Some ty -> ty
    | None -> untypeable id ("Invalid domain for constructor.")
    end
  | Lambda (_, v, e), ALambda (s, annot) ->
    let env = Env.add v (TyScheme.mk_mono s) env in
    let t = typeof env annot e in
    let lb = Arrow.mk (GTy.ub s) (GTy.lb t) in
    let ub = Arrow.mk (GTy.lb s) (GTy.ub t) in
    GTy.mk_gradual lb ub
  | LambdaRec lst, ALambdaRec anns when List.length lst = List.length anns ->
    let lst = List.combine lst anns in
    let env = lst |> List.fold_left
      (fun env ((_,v,_),(ty,_)) -> Env.add v (TyScheme.mk_mono ty) env) env in
    let tys = lst |> List.map (fun ((_,_,e),(ty,annot)) -> typeof env annot e, ty) in
    if List.for_all (fun (ty, ty') -> Ty.leq (GTy.lb ty) (GTy.lb ty')) tys
    then tys |> List.map fst |> GTy.mapl Tuple.mk
    else untypeable id ("Invalid recursive lambda.")
  | Ite (e, tau, e1, e2), AIte (annot, b1, b2) ->
    let s = typeof env annot e in
    let t1 = typeof_b env b1 e1 s tau in
    let t2 = typeof_b env b2 e2 s (Ty.neg tau) in
    GTy.cup t1 t2
  | Alt _, AAlt (None, None) ->
    untypeable id ("At least one side of a Alt expr must be typeable.")
  | Alt (e1, e2), AAlt (a1, a2) ->
    let t1 = match a1 with None -> GTy.any | Some a1 -> typeof env a1 e1 in
    let t2 = match a2 with None -> GTy.any | Some a2 -> typeof env a2 e2 in
    GTy.cap t1 t2
  | App (e1, e2), AApp (annot1, annot2) ->
    let t1 = typeof env annot1 e1 in
    let t2 = typeof env annot2 e2 in
    app t1 t2
  | Operation (o, e), AOp (s, annot) ->
    let t1 = fun_of_operation o |> subst_ts s in
    let t2 = typeof env annot e in
    app t1 t2
  | Projection (p, e), AProj annot ->
    let dom = domain_of_proj p Ty.any in
    let check ty = Ty.leq ty dom in
    let t = typeof env annot e in
    begin match GTy.op check (proj p) t with
    | Some ty -> ty
    | None -> untypeable id "Invalid projection."
    end
  | Let (_, v, e1, e2), ALet (annot1, annots2) ->
    let tvs,s = typeof_def env annot1 e1 |> TyScheme.get in
    let aux (si, annot) =
      let si = GTy.mk si in
      if MVarSet.inter tvs (GTy.fv si) |> MVarSet.is_empty then
        let s = TyScheme.mk tvs (GTy.cap s si) in
        typeof (Env.add v s env) annot e2
      else
        untypeable id ("Partition of "^(Variable.show v)^" contains generalized variables.")
    in
    if Ty.leq (GTy.ub s) (List.map fst annots2 |> Ty.disj) then
      List.map aux annots2 |> GTy.disj
    else
      untypeable id ("Partition does not cover the type of "^(Variable.show v)^".")
  | TypeCast (e, _, c), ACast (ty, annot) ->
    let t = typeof env annot e in
    if (c = Check && GTy.leq t ty)
    || (c = CheckStatic && Ty.leq (GTy.lb t) (GTy.lb ty))
    || (c = NoCheck)
    then GTy.cap t ty
    else untypeable id "Type constraint not satisfied."
  | TypeCoerce (e, _, c), ACoerce (ty, annot) ->
    let t = typeof env annot e in
    if (c = Check && GTy.leq t ty)
    || (c = CheckStatic && Ty.leq (GTy.lb t) (GTy.lb ty))
    || (c = NoCheck)
    then ty
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
    let ty = typeof' env annot.ann e in
    annot.cache <- Some ty ;
    ty
and typeof_b env bannot (id,e) s tau =
  match bannot with
  | BType annot -> typeof env annot (id,e)
  | BSkip ->
    if Ty.disjoint (GTy.ub s) tau |> not
    then untypeable id "Branch is reachable and must be typed." ;
    GTy.empty
and typeof_def env annot e =
  typeof env annot e |> generalize ~e env
