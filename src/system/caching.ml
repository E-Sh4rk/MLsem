open Types.Base
open Types.Tvar
open Parsing.Variable
open Env
open Annot

(* TODO: refactor *)
let norm_tagcomp c =
  let open Sstt.Extensions.Abstracts in
  match destruct c with
  | None -> c
  | Some (tag, dnf) ->
    let vs = params_of tag in
    let has_no_empty lst =
      List.for_all2 (fun v ty -> v <> Inv || non_empty ty) vs lst
    in
    let has_no_empty (ps,_) =
      List.for_all has_no_empty ps
    in
    let dnf = List.filter has_no_empty dnf in
    let mk_line (ps,ns) =
      let ps = ps |> List.map (fun lst -> mk tag lst) |> conj in
      let ns = ns |> List.map (fun lst -> mk tag lst) |> List.map neg |> conj in
      cap ps ns
    in
    dnf |> List.map mk_line |> disj |> Sstt.Ty.get_descr |> Sstt.Descr.get_tags
    |> Sstt.Tags.get tag
let norm_tags t = Sstt.Tags.map norm_tagcomp t
let norm_descr d =
  let open Sstt.Descr in
  d |> components |> List.map (function
    | Intervals i -> Intervals i
    | Atoms a -> Atoms a
    | Tags t -> Tags (norm_tags t)
    | Arrows a -> Arrows a
    | Tuples t -> Tuples t
    | Records r -> Records r
  ) |> of_components
let norm_vdescr = Sstt.VDescr.map norm_descr
let norm = Sstt.Transform.transform norm_vdescr

module Domain = struct
  type t = IAnnot.coverage list
  [@@deriving show]

  let empty = []
  let add c t = c::t

  let env_to_typ ?(normalize=false) renv =
    let f = if normalize then norm else Utils.identity in
    let bindings = renv
      |> REnv.bindings |> List.map
        (fun (v, ty) -> (Variable.get_unique_name v, (false, f ty)))
    in
    mk_record true bindings

  let more_specific res1 res2 =
    match res1, res2 with
    | _, None -> true
    | None, _ -> false
    | Some (eid1,ty1), Some (eid2,ty2) when eid1=eid2 ->
      subtype ty1 ty2
    | Some _, Some _ -> false

  let covers t (res,renv) =
    let renvs = t
      |> List.filter (fun (res',_) -> more_specific res' res)
      |> List.map (fun (_,renv) -> renv)
    in
    let a = renvs |> List.map env_to_typ |> disj in
    let b = env_to_typ ~normalize:true renv in
    subtype b a
end

module Cache = struct
  type 'a t = (Parsing.Ast.exprid * IAnnot.t, (Env.t * TVarSet.t * 'a) list) Hashtbl.t

  let empty () = Hashtbl.create 100

  let add (id,e) env a res t =
    let tvars = Env.tvars env in (* Required due to tvar caching *)
    let env = Env.restrict (Ast.fv (id,e) |> VarSet.elements) env in
    let lst = match Hashtbl.find_opt t (id,a) with Some lst -> lst | None -> [] in
    let lst = (env,tvars,res)::lst in
    Hashtbl.replace t (id,a) lst

  let get (id,e) env a t =
    match Hashtbl.find_opt t (id,a) with
    | None -> None
    | Some lst ->
      let tvars = Env.tvars env in
      let env = Env.restrict (Ast.fv (id,e) |> VarSet.elements) env in
      lst |> List.find_opt
        (fun (env',tvars',_) -> TVarSet.equal tvars tvars' && Env.equiv env env')
      |> Option.map (fun (_,_,r) -> r)
end

module TVCache = struct
  type t = (Parsing.Ast.exprid * TVar.t, TVar.t) Hashtbl.t

  let empty () = Hashtbl.create 100

  let get h eid tv =
    match Hashtbl.find_opt h (eid, tv) with
    | Some tv -> tv
    | None ->
      let tv' = TVar.mk None in
      Hashtbl.replace h (eid, tv) tv' ; tv'

  let get' t eid tvs =
    TVarSet.destruct tvs
    |> List.map (fun tv -> tv, get t eid tv |> TVar.typ)
    |> Subst.construct
  
  let res_tvar = TVar.mk None
  let res_tvars = Hashtbl.create 5
  let res_tvar' i =
    match Hashtbl.find_opt res_tvars i with
    | Some tv -> tv
    | None ->
      let tv = TVar.mk None in
      Hashtbl.replace res_tvars i tv ; tv
end