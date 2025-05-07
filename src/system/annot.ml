open Types.Base
open Types.Tvar

module Annot = struct
  type branch = BType of t | BSkip
  [@@deriving show]
  and inter = t list
  [@@deriving show]
  and part = (typ * t) list
  [@@deriving show]
  and a =
  | AConst | AAbstract | AAtom
  | AAx of Subst.t
  | ALet of t * part
  | AApp of t * t | ACons of t * t
  | AProj of t | ATag of t | AConstr of t | ACoerce of t
  | AUpdate of t * t option
  | ATuple of t list
  | AIte of t * branch * branch
  | ALambda of typ * t
  | AInter of inter
  [@@deriving show]
  and t = { mutable cache: typ option ; ann: a }
  [@@deriving show]

  let substitute s t =
    let rec aux t =
      let ann = match t.ann with
      | AConst -> AConst | AAbstract -> AAbstract | AAtom -> AAtom
      | AAx s' -> AAx (Subst.compose_restr s s')
      | ALet (t, ps) -> ALet (aux t, List.map (fun (ty, t) -> Subst.apply s ty, aux t) ps)
      | AApp (t1, t2) -> AApp (aux t1, aux t2)
      | ACons (t1, t2) -> ACons (aux t1, aux t2)
      | AProj t -> AProj (aux t) | ATag t -> ATag (aux t)
      | AConstr t -> AConstr (aux t) | ACoerce t -> ACoerce (aux t)
      | AUpdate (t, ot) -> AUpdate (aux t, Option.map aux ot)
      | ATuple ts -> ATuple (List.map aux ts)
      | AIte (t,b1,b2) -> AIte (aux t, aux_b b1, aux_b b2)
      | ALambda (ty, t) -> ALambda (Subst.apply s ty, aux t)
      | AInter ts -> AInter (List.map aux ts)
    in { cache=Option.map (Subst.apply s) t.cache ; ann }
    and aux_b b =
      match b with BSkip -> BSkip | BType t -> BType (aux t)  
    in
    aux t

  let tvars t =
    let rec aux t =
      let vs = match t.ann with
        | AConst | AAbstract | AAtom -> []
        | AAx s -> [Subst.vars s]
        | ALet (t, ps) ->
          (aux t)::(List.map (fun (s, t) -> TVarSet.union (vars s) (aux t)) ps)
        | AApp (t1, t2) | ACons (t1, t2) | AUpdate (t1, Some t2) -> [aux t1 ; aux t2]
        | AProj t | ATag t | AConstr t | ACoerce t | AUpdate (t, None) -> [aux t]
        | ATuple ts | AInter ts -> List.map aux ts
        | AIte (t,b1,b2) -> [ aux t ; aux_b b1 ; aux_b b2 ]
        | ALambda (ty, t) -> [ vars ty ; aux t ]
        in
        TVarSet.union_many vs
    and aux_b b =
      match b with BSkip -> TVarSet.empty | BType t -> aux t
    in
    aux t

  let nc a = { cache=None ; ann=a }
end

module IAnnot = struct
  type branch = BType of t | BSkip | BInfer
  [@@deriving show]
  and inter = t list
  [@@deriving show]
  and part = (typ * t) list
  [@@deriving show]
  and t =
  | A of Annot.t
  | Infer
  | Untyp
  | AConst | AAbstract | AAtom
  | AAx of Subst.t
  | ALet of t * part
  | AApp of t * t | ACons of t * t
  | AProj of t | ATag of t | AConstr of t | ACoerce of t
  | AUpdate of t * t option
  | ATuple of t list
  | AIte of t * branch * branch
  | ALambda of typ * t
  | AInter of inter
  [@@deriving show]

  let substitute s t =
    let rec aux t =
      match t with
      | A a -> A (Annot.substitute s a)
      | Infer -> Infer
      | Untyp -> Untyp
      | AConst -> AConst | AAbstract -> AAbstract | AAtom -> AAtom
      | AAx s' -> AAx (Subst.compose_restr s s')
      | ALet (t, ps) -> ALet (aux t, List.map (fun (ty, t) -> Subst.apply s ty, aux t) ps)
      | AApp (t1, t2) -> AApp (aux t1, aux t2)
      | ACons (t1, t2) -> ACons (aux t1, aux t2)
      | AProj t -> AProj (aux t) | ATag t -> ATag (aux t)
      | AConstr t -> AConstr (aux t) | ACoerce t -> ACoerce (aux t)
      | AUpdate (t, ot) -> AUpdate (aux t, Option.map aux ot)
      | ATuple ts -> ATuple (List.map aux ts)
      | AIte (t,b1,b2) -> AIte (aux t, aux_b b1, aux_b b2)
      | ALambda (ty, t) -> ALambda (Subst.apply s ty, aux t)
      | AInter ts -> AInter (List.map aux ts)
    and aux_b b =
      match b with
      | BType t -> BType (aux t)
      | BInfer -> BInfer | BSkip -> BSkip
    in
    aux t

  let tvars t =
    let rec aux t =
      let vs = match t with
        | A t -> [Annot.tvars t]
        | Infer | Untyp | AConst | AAbstract | AAtom -> []
        | AAx s -> [Subst.vars s]
        | ALet (t, ps) ->
          (aux t)::(List.map (fun (s, t) -> TVarSet.union (vars s) (aux t)) ps)
        | AApp (t1, t2) | ACons (t1, t2) | AUpdate (t1, Some t2) -> [aux t1 ; aux t2]
        | AProj t | ATag t | AConstr t | ACoerce t | AUpdate (t, None) -> [aux t]
        | ATuple ts | AInter ts -> List.map aux ts
        | AIte (t,b1,b2) -> [ aux t ; aux_b b1 ; aux_b b2 ]
        | ALambda (ty, t) -> [ vars ty ; aux t ]
      in
      TVarSet.union_many vs
    and aux_b b = match b with
      | BInfer | BSkip -> TVarSet.empty
      | BType t -> aux t
    in
    aux t
end