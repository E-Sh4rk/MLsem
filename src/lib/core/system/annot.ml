open Common
open Types

module Annot = struct
  type branch = BType of t | BSkip
  [@@deriving show]
  and inter = t list
  [@@deriving show]
  and part = (Ty.t * t) list
  [@@deriving show]
  and a =
  | AConst
  | AAbstract of GTy.t
  | AAx of Subst.t
  | AConstruct of t list
  | ALet of t * part
  | AApp of t * t
  | AProj of t
  | ACast of t
  | ACoerce of GTy.t * t
  | AIte of t * branch * branch
  | ACf of t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AInter of inter
  [@@deriving show]
  and t = { mutable cache: GTy.t option ; ann: a }
  [@@deriving show]

  let substitute s t =
    let rec aux t =
      let ann = match t.ann with
      | AConst -> AConst
      | AAbstract t -> AAbstract (GTy.substitute s t)
      | AAx s' -> AAx (Subst.compose_restr s s')
      | AConstruct ts -> AConstruct (List.map aux ts)
      | ALet (t, ps) -> ALet (aux t, List.map (fun (ty, t) -> Subst.apply s ty, aux t) ps)
      | AApp (t1, t2) -> AApp (aux t1, aux t2)
      | AProj t -> AProj (aux t)
      | ACast t -> ACast (aux t)
      | ACoerce (ty, t) -> ACoerce (GTy.substitute s ty, aux t)
      | AIte (t,b1,b2) -> AIte (aux t, aux_b b1, aux_b b2)
      | ACf (t,b1,b2) -> ACf (aux t, aux_b b1, aux_b b2)
      | ALambda (ty, t) -> ALambda (GTy.substitute s ty, aux t)
      | ALambdaRec lst -> ALambdaRec (List.map (fun (ty,t) -> (GTy.substitute s ty, aux t)) lst)
      | AInter ts -> AInter (List.map aux ts)
    in { cache=Option.map (GTy.substitute s) t.cache ; ann }
    and aux_b b =
      match b with BSkip -> BSkip | BType t -> BType (aux t)  
    in
    aux t

  let nc a = { cache=None ; ann=a }
end

module IAnnot = struct
  type coverage = (Eid.t * Ty.t) option * REnv.t
  [@@deriving show]
  type branch = BType of t | BSkip | BInfer
  [@@deriving show]
  and inter_branch = { coverage: coverage option ; ann: t }
  [@@deriving show]
  and inter = inter_branch list
  [@@deriving show]
  and part = (Ty.t * t) list
  [@@deriving show]
  and t =
  | A of Annot.t
  | Infer
  | Untyp
  | AConstruct of t list
  | ALet of t * part
  | AApp of t * t
  | AProj of t
  | ACast of t
  | ACoerce of GTy.t * t
  | AIte of t * branch * branch
  | ACf of t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AInter of inter
  [@@deriving show]

  let substitute s =
    let rec aux t =
      match t with
      | A a -> A (Annot.substitute s a)
      | Infer -> Infer
      | Untyp -> Untyp
      | AConstruct ts -> AConstruct (List.map aux ts)
      | ALet (t, ps) -> ALet (aux t, List.map (fun (ty, t) -> Subst.apply s ty, aux t) ps)
      | AApp (t1, t2) -> AApp (aux t1, aux t2)
      | AProj t -> AProj (aux t)
      | ACast t -> ACast (aux t)
      | ACoerce (ty, t) -> ACoerce (GTy.substitute s ty, aux t)
      | AIte (t,b1,b2) -> AIte (aux t, aux_b b1, aux_b b2)
      | ACf (t,b1,b2) -> ACf (aux t, aux_b b1, aux_b b2)
      | ALambda (ty, t) -> ALambda (GTy.substitute s ty, aux t)
      | ALambdaRec lst -> ALambdaRec (List.map (fun (ty,t) -> (GTy.substitute s ty, aux t)) lst)
      | AInter bs -> AInter (List.map aux_ib bs)
    and aux_b b =
      match b with
      | BType t -> BType (aux t)
      | BInfer -> BInfer | BSkip -> BSkip
    and aux_ib { coverage ; ann } =
      let aux_coverage (o,renv) =
        let o = o |> Option.map (fun (eid,ty) -> (eid, Subst.apply s ty)) in
        let renv = REnv.substitute s renv in
        (o, renv)
      in
      let coverage = Option.map aux_coverage coverage in
      let ann = aux ann in
      { coverage ; ann }
    in
    aux
end