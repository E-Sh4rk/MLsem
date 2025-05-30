open Types.Base
open Types.Tvar
open Parsing.Variable
open Env
open Annot

module Domain = struct
  type t = IAnnot.coverage list
  [@@deriving show]

  let empty = []
  let add c t = c::t

  let env_to_typ renv =
    let bindings = renv
      |> REnv.bindings |> List.map
        (fun (v, ty) -> (Variable.get_unique_name v, (false, ty)))
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
    let b = env_to_typ renv in
    subtype b a
end

module Cache = struct
  type 'a t = (Parsing.Ast.exprid * IAnnot.t, (Env.t * 'a) list) Hashtbl.t

  let empty () = Hashtbl.create 100

  let add (id,e) env a res t =
    let env = Env.restrict (Ast.fv (id,e) |> VarSet.elements) env in
    let lst = match Hashtbl.find_opt t (id,a) with Some lst -> lst | None -> [] in
    let lst = (env,res)::lst in
    Hashtbl.replace t (id,a) lst

  let get (id,e) env a t =
    match Hashtbl.find_opt t (id,a) with
    | None -> None
    | Some lst ->
      let env = Env.restrict (Ast.fv (id,e) |> VarSet.elements) env in
      List.find_opt (fun (env',_) -> Env.equiv env env') lst |> Option.map snd
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
end