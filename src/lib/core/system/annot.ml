open Mlsem_common
open Mlsem_types

module Annot = struct
  type branch = BType of t | BSkip
  [@@deriving show]
  and inter = t list
  [@@deriving show]
  and part = (Ty.t * t) list
  [@@deriving show]
  and a =
  | AValue of GTy.t
  | AVar of Subst.t 
  | AConstruct of t list
  | ALet of t * part
  | AApp of t * t
  | AOp of Subst.t * t
  | AProj of t
  | ACast of GTy.t * t
  | ACoerce of GTy.t * t
  | AIte of t * GTy.t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AAlt of t option * t option
  | AInter of inter
  [@@deriving show]
  and t = { mutable cache: GTy.t option ; ann: a }
  [@@deriving show]

  let substitute s t =
    let comp s' = Subst.compose_restr s s' in
    let rec aux t =
      let ann = match t.ann with
      | AValue t -> AValue (GTy.substitute s t)
      | AVar s' -> AVar (comp s')
      | AConstruct ts -> AConstruct (List.map aux ts)
      | ALet (t, ps) -> ALet (aux t, List.map (fun (ty, t) -> Subst.apply s ty, aux t) ps)
      | AApp (t1, t2) -> AApp (aux t1, aux t2)
      | AOp (s', t) -> AOp (comp s', aux t)
      | AProj t -> AProj (aux t)
      | ACast (ty, t) -> ACast (GTy.substitute s ty, aux t)
      | ACoerce (ty, t) -> ACoerce (GTy.substitute s ty, aux t)
      | AIte (t,ty,b1,b2) -> AIte (aux t, GTy.substitute s ty, aux_b b1, aux_b b2)
      | ALambda (ty, t) -> ALambda (GTy.substitute s ty, aux t)
      | ALambdaRec lst -> ALambdaRec (List.map (fun (ty,t) -> (GTy.substitute s ty, aux t)) lst)
      | AAlt (t1, t2) -> AAlt (Option.map aux t1, Option.map aux t2)
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
  type branch = BType of bool (* explored *) * t | BSkip
  [@@deriving show]
  and inter_branch = { coverage: coverage option ; ann: t }
  [@@deriving show]
  and inter = inter_branch list
  [@@deriving show]
  and part = (Ty.t * t) list
  [@@deriving show]
  and t =
  | A of Annot.t
  | Untyp
  | AVar of (MVarSet.t -> Subst.t)
  | AConstruct of t list
  | ALet of t * part
  | AApp of t * t * Ty.t (* result *)
  | AOp of (MVarSet.t -> Subst.t) * t * Ty.t (* result *)
  | AProj of t * Ty.t (* result *)
  | ACast of GTy.t * t
  | ACoerce of GTy.t * t
  | AIte of t * GTy.t * branch * branch
  | ALambda of GTy.t * t
  | ALambdaRec of (GTy.t * t) list
  | AAlt of t option * t option
  | AInter of inter
  [@@deriving show]

  let substitute s =
    let rec aux t =
      match t with
      | A a -> A (Annot.substitute s a)
      | Untyp -> Untyp
      | AVar f -> AVar f
      | AConstruct ts -> AConstruct (List.map aux ts)
      | ALet (t, ps) -> ALet (aux t, List.map (fun (ty, t) -> Subst.apply s ty, aux t) ps)
      | AApp (t1, t2, ty) -> AApp (aux t1, aux t2, Subst.apply s ty)
      | AOp (f, t, ty) -> AOp (f, aux t, Subst.apply s ty)
      | AProj (t, ty) -> AProj (aux t, Subst.apply s ty)
      | ACast (ty, t) -> ACast (GTy.substitute s ty, aux t)
      | ACoerce (ty, t) -> ACoerce (GTy.substitute s ty, aux t)
      | AIte (t,ty,b1,b2) -> AIte (aux t, GTy.substitute s ty, aux_b b1, aux_b b2)
      | ALambda (ty, t) -> ALambda (GTy.substitute s ty, aux t)
      | ALambdaRec lst -> ALambdaRec (List.map (fun (ty,t) -> (GTy.substitute s ty, aux t)) lst)
      | AAlt (t1, t2) -> AAlt (Option.map aux t1, Option.map aux t2)
      | AInter bs -> AInter (List.map aux_ib bs)
    and aux_b b =
      match b with
      | BType (b,t) -> BType (b,aux t)
      | BSkip -> BSkip
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

module Domain = struct
  type t = IAnnot.coverage list
  [@@deriving show]

  let transform_abstract =
    let aux (_, dnf) =
      let has_no_empty lst =
        List.for_all Ty.non_empty lst
      in
      let has_no_empty (ps,_) =
        List.for_all has_no_empty ps
      in
      List.filter has_no_empty dnf
    in
    Abstract.transform aux

  let empty = []
  let add c t = c::t

  let env_to_typ ?(normalize=false) renv =
    let f = if normalize then transform_abstract else Fun.id in
    let bindings = renv
      |> REnv.bindings |> List.map
        (fun (v, ty) -> (Variable.get_unique_name v, (f ty, false)))
    in
    Record.mk_open bindings

  let more_specific res1 res2 =
    match res1, res2 with
    | _, None -> true
    | None, _ -> false
    | Some (eid1,ty1), Some (eid2,ty2) when eid1=eid2 ->
      Ty.leq ty1 ty2
    | Some _, Some _ -> false

  let covers t (res,renv) =
    let renvs = t
      |> List.filter (fun (res',_) -> more_specific res' res)
      |> List.map (fun (_,renv) -> renv)
    in
    let a = renvs |> List.map env_to_typ |> Ty.disj in
    let b = env_to_typ ~normalize:(!Config.no_empty_param) renv in
    Ty.leq b a
end
