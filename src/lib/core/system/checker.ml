open Mlsem_common
open Mlsem_types
open Annot
open Ast

(* Auxiliary *)

let is_type_test_unsat ~tau t =
  let ntau = GTy.neg tau in
  if GTy.non_gradual ntau && GTy.non_gradual t
  then Ty.diff (GTy.ub t) (GTy.lb ntau) |> !Config.normalization_fun
  else
    let norm1 = Ty.diff (GTy.lb t) (GTy.lb ntau) |> !Config.normalization_fun in
    let norm2 = Ty.diff (GTy.ub t) (GTy.ub ntau) |> !Config.normalization_fun in
    Ty.cup norm1 norm2

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
  | Join _ | Meet _  | Negate | Ternary _ | Normalize -> true
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
  let app t1 t2 res =
    if Ty.leq (GTy.lb t1) (Arrow.mk (GTy.lb t2) res)
    then
      if GTy.non_gradual t1 && GTy.non_gradual t2 then GTy.mk res
      else GTy.mk_gradual res (Arrow.apply (GTy.ub t1) (GTy.ub t2))
    else untypeable id "Invalid application."
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
    (* ISSUE: uncommenting below fixes the issue (EDIT: not anymore) *)
    (* let tys = tys |> List.map (fun (ty1,ty2) -> GTy.normalize ty1, GTy.normalize ty2) in *)
    tys |> List.iter (fun (ty,ty') -> Format.printf "%a@.<=@.%a@.@." Ty.pp (GTy.lb ty) Ty.pp (GTy.lb ty')) ;
    if List.for_all (fun (ty, ty') -> Ty.leq (GTy.lb ty) (GTy.lb ty')) tys
    then tys |> List.map fst |> GTy.mapl Tuple.mk
    else untypeable id ("Invalid recursive lambda.")
  | Ite (e, _, e1, e2), AIte (annot, tau, b1, b2) ->
    let s = typeof env annot e in
    let t1 = typeof_b env b1 e1 s tau in
    let t2 = typeof_b env b2 e2 s (GTy.neg tau) in
    GTy.cup t1 t2
  | Alt _, AAlt (None, None) ->
    untypeable id ("At least one side of a Alt expr must be typeable.")
  | Alt (e1, e2), AAlt (a1, a2) ->
    let t1 = match a1 with None -> GTy.any | Some a1 -> typeof env a1 e1 in
    let t2 = match a2 with None -> GTy.any | Some a2 -> typeof env a2 e2 in
    GTy.cap t1 t2
  | App (e1, e2), AApp (annot1, annot2, res) ->
    let t1 = typeof env annot1 e1 in
    let t2 = typeof env annot2 e2 in
    app t1 t2 res
  | Operation (o, e), AOp (s, annot, res) ->
    let t1 = fun_of_operation o |> subst_ts s in
    let t2 = typeof env annot e in
    app t1 t2 res
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
      if MVarSet.inter tvs (TVOp.vars si) |> MVarSet.is_empty then
        match annot with
        | None when Ty.is_empty (Ty.cap (GTy.ub s) si) -> GTy.empty
        | None -> untypeable id ("Part of "^(Variable.show v)^" is non-empty and should be typed.")
        | Some annot ->
            let si = GTy.mk si in
            let s = TyScheme.mk tvs (GTy.cap s si) in
            typeof (Env.add v s env) annot e2
      else
        untypeable id ("Partition of "^(Variable.show v)^" contains generalized variables.")
    in
    List.map aux annots2 |> GTy.disj
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
    let env = REnv.refine_env env annot.refinement in
    let ty = typeof' env annot.ann e in
    annot.cache <- Some ty ;
    ty
and typeof_b env bannot (id,e) s tau =
  match bannot with
  | BType annot -> typeof env annot (id,e)
  | BSkip ->
    if is_type_test_unsat ~tau s |> Ty.is_empty |> not
    then untypeable id "Branch is reachable and must be typed." ;
    GTy.empty
and typeof_def env annot e =
  typeof env annot e |> generalize ~e env
