open Annot
open Types.Base
open Types.Tvar
open Types
open Env
open Ast

type result =
| Ok of Annot.t * typ
| Fail
| Subst of Subst.t list * IAnnot.t * IAnnot.t

let rec infer env annot (id, e) =
  let open IAnnot in
  match e, annot with
  | _, A a -> Ok (a, Checker.typeof env a (id, e))
  | _, Untyp -> Fail
  | Abstract _, Infer -> infer env (A Annot.AAbstract) (id, e)
  | Const _, Infer -> infer env (A Annot.AConst) (id, e)
  | Var v, Infer ->
    let (tvs,_) = Env.find v env |> TyScheme.get in
    let s = refresh tvs in
    infer env (A (Annot.AAx s)) (id, e)
  | Atom _, Infer -> infer env (A Annot.AAtom) (id, e)
  | Tag _, Infer -> infer env (ATag Infer) (id, e)
  | Tag (_, e'), ATag annot' ->
    begin match infer' env annot' e' with
    | Ok (annot', _) -> infer env (A (Annot.ATag annot')) (id, e)
    | Subst (ss,a1,a2) -> Subst (ss,ATag a1,ATag a2)
    | Fail -> Fail
    end
  | Lambda (tys,_,_), Infer ->
    let refresh_internal ty =
      let s = refresh (vars_internal ty) in
      Subst.apply s ty
    in
    let tys = List.map refresh_internal tys in
    let branches = List.map (fun ty -> ALambda (ty, Infer)) tys in
    infer env (AInter branches) (id, e)
  | Lambda (_,v,e'), ALambda (ty, annot') ->
    let env' = Env.add v (TyScheme.mk_mono ty) env in
    begin match infer' env' annot' e' with
    | Ok (annot', _) -> infer env (A (Annot.ATag annot')) (id, e)
    | Subst (ss,a1,a2) -> Subst (ss,ALambda (ty, a1),ALambda (ty, a2))
    | Fail -> Fail
    end
  | Ite _, Infer -> infer env (AIte (Infer, BInfer, BInfer)) (id, e)
  | Ite (e0,tau,e1,e2), AIte (a0,a1,a2) ->
    begin match infer' env a0 e0 with
    | Fail -> Fail
    | Subst (ss,a,a') -> Subst (ss,AIte (a,a1,a2),AIte (a',a1,a2))
    | Ok (a0, s) ->
      begin match a1, a2 with
      | BInfer, a2 ->
        let ss = tallying (TVar.user_vars ()) [(s,neg tau)] in
        Subst (ss, AIte(A a0,BSkip,a2), AIte(A a0,BType Infer,a2))
      | a1, BInfer ->
        let ss = tallying (TVar.user_vars ()) [(s,tau)] in
        Subst (ss, AIte(A a0,a1,BSkip), AIte(A a0,a1,BType Infer))
      | a1, a2 -> failwith "TODO"
      end
    end
  | _, _ -> failwith "TODO"
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

let infer env e =
  match infer env IAnnot.Infer e with
  | Fail -> None
  | Subst _ -> assert false
  | Ok (a,_) -> Some a
