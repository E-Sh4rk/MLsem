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
  | AApp of t * t * Ty.t (* result *)
  | AOp of Subst.t * t * Ty.t (* result *)
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
      | AApp (t1, t2, ty) -> AApp (aux t1, aux t2, Subst.apply s ty)
      | AOp (s', t, ty) -> AOp (comp s', aux t, Subst.apply s ty)
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

module rec IAnnot : sig
  type coverage = (Eid.t * Ty.t) option * REnv.t
  type branch = BMaybe of t | BType of t | BSkip
  and inter_branch = { coverage: coverage option ; ann: t }
  and inter = inter_branch list
  and part = (Ty.t * LazyIAnnot.t) list
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

  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val pp_coverage : Format.formatter -> coverage -> unit
end = struct
  type coverage = (Eid.t * Ty.t) option * REnv.t
  [@@deriving show]
  type branch = BMaybe of t | BType of t | BSkip
  [@@deriving show]
  and inter_branch = { coverage: coverage option ; ann: t }
  [@@deriving show]
  and inter = inter_branch list
  [@@deriving show]
  and part = (Ty.t * LazyIAnnot.t) list
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
      | ALet (t, ps) ->
        ALet (aux t, List.map (fun (ty, t) -> Subst.apply s ty, LazyIAnnot.substitute s t) ps)
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
      | BMaybe t -> BMaybe (aux t)
      | BType t -> BType (aux t)
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
and LazyIAnnot : sig
  type t
  val get : t -> IAnnot.t
  val mk_lazy : (unit -> IAnnot.t) -> t
  val mk : IAnnot.t -> t
  val is_concrete : t -> bool
  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type ann' =
  | Concrete of IAnnot.t
  | Potential of (unit -> IAnnot.t)
  type ann = ann' ref
  type v = { ann : ann ; ss : Subst.t list }
  type t = v ref

  let force (ann:ann) : IAnnot.t =
    let iann = match !ann with
    | Concrete ann -> ann
    | Potential f -> f ()
    in
    ann := Concrete iann ; iann
  let get (t:t) =
    let ann = (!t).ann |> force |> List.fold_right IAnnot.substitute (!t).ss in
    t := { ann=ref (Concrete ann) ; ss=[] } ; ann
  let mk_lazy f : t = ref { ann=ref (Potential f) ; ss=[] }
  let mk ann : t = ref { ann=ref (Concrete ann) ; ss=[] }
  let substitute s (t:t) : t =
    ref { ann=(!t).ann ; ss=s::(!t).ss }
  let is_concrete (t:t) =
    match !(!t.ann) with Concrete _ -> true | Potential _ -> false
  let pp fmt (t:t) =
    match !(!t.ann) with
    | Concrete t -> IAnnot.pp fmt t
    | Potential _ -> Format.fprintf fmt "_"
end

module Domain = struct
  type t = IAnnot.coverage list
  [@@deriving show]

  let empty = []
  let add c t = c::t

  let env_to_typ renv =
    let bindings = renv
      |> REnv.bindings |> List.map
        (fun (v, ty) -> (Variable.get_unique_name v, (ty, false)))
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
    let b = env_to_typ renv in
    Ty.is_empty (Ty.diff b a |> !Config.normalization_fun)
end
