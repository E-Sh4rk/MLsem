open Common
open Types
open Annot

module Domain = struct
  type t = IAnnot.coverage list
  [@@deriving show]

  let transform_abstract =
    let aux (abs, dnf) =
      let vs = Abstract.params abs in
      let has_no_empty lst =
        List.for_all2 (fun v ty -> v <> Abstract.Inv || Ty.non_empty ty) vs lst
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
        (fun (v, ty) -> (Variable.get_unique_name v, (false, f ty)))
    in
    Record.mk true bindings

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

module TVCache = struct
  type t = { expr: (Eid.t * TVar.t, TVar.t) Hashtbl.t ;
             abs: (Abstract.t * int * TVar.t, TVar.t) Hashtbl.t }

  let empty () =
    { expr = Hashtbl.create 100 ; abs = Hashtbl.create 100 }

  let get t eid tv =
    match Hashtbl.find_opt t.expr (eid, tv) with
    | Some tv -> tv
    | None ->
      let tv' = TVar.mk None in
      Hashtbl.replace t.expr (eid, tv) tv' ; tv'

  let get' t eid tvs =
    TVarSet.destruct tvs
    |> List.map (fun tv -> tv, get t eid tv |> TVar.typ)
    |> Subst.construct

  let get_abs_param t abs i tv =
    match Hashtbl.find_opt t.abs (abs, i, tv) with
    | Some tv -> tv
    | None ->
      let tv' = TVar.mk None in
      Hashtbl.replace t.abs (abs, i, tv) tv' ; tv'
  
  let res_tvar = TVar.mk None
  let res_tvars = Hashtbl.create 5
  let res_tvar' i =
    match Hashtbl.find_opt res_tvars i with
    | Some tv -> tv
    | None ->
      let tv = TVar.mk None in
      Hashtbl.replace res_tvars i tv ; tv
end