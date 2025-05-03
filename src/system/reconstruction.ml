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
    | Subst (s,a1,a2) -> Subst (s,ATag a1,ATag a2)
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
    | Subst (s,a1,a2) -> Subst (s,ALambda (ty, a1),ALambda (ty, a2))
    | Fail -> Fail
    end
  | _, _ -> failwith "TODO"
and infer' env annot e =
  let mono = Env.tvars env in
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
      let tvs = TVarSet.filter (fun tv -> TVar.from_user tv |> not) tvs in
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
