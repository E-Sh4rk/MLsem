open Parsing.Variable
open Types.Base
open Types.Tvar

module TyScheme = struct
  type t = TVarSet.t * typ
  let mk tvs ty =
    let tvs = TVarSet.inter tvs (vars ty) in
    (tvs, ty)
  let mk_mono ty = mk TVarSet.empty ty
  let mk_poly ty = mk (vars ty) ty
  let get (tvs, ty) = (tvs, ty)
  let fv (tvs, ty) = TVarSet.diff (vars ty) tvs
  let leq (tvs1,ty1) (tvs2,ty2) =
    TVarSet.subset tvs2 tvs1 &&
    subtype ty1 ty2
  let equiv t1 t2 = leq t1 t2 && leq t2 t1
  let leq_inst (tvs1,ty1) (_,ty2) =
    let s = refresh tvs1 in
    let mono = TVarSet.union (vars ty1) (vars ty2) in
    let ty1 = Subst.apply s ty1 in
    test_tallying mono [ty1,ty2]
  let equiv_inst t1 t2 = leq_inst t1 t2 && leq_inst t2 t1
  let pp fmt (tvs, ty) =
    Format.fprintf fmt "∀%a.%a"
      (Utils.pp_list TVar.pp) (TVarSet.destruct tvs) pp_typ ty
  let pp_short fmt (tvs, ty) =
    let s = shorten_names tvs in
    let ty = Subst.apply s ty in
    let tvs = TVarSet.destruct tvs
      |> List.map TVar.typ
      |> List.map (Subst.apply s) in
    Format.fprintf fmt "∀%a.%a"
      (Utils.pp_list pp_typ) tvs pp_typ ty
end

module Env = struct
  type t = TyScheme.t VarMap.t * TVarSet.t

  let empty = (VarMap.empty, TVarSet.empty)
  let is_empty (m,_) =  VarMap.is_empty m
  let singleton v t = (VarMap.singleton v t, TyScheme.fv t)
  let construct lst = (VarMap.of_seq (List.to_seq lst),
    List.map snd lst |> List.map TyScheme.fv |> TVarSet.union_many)

  let add v t (m,s) = (VarMap.add v t m, TVarSet.union s (TyScheme.fv t))

  let domain (m, _) = VarMap.bindings m |> List.map fst

  let bindings (m, _) = VarMap.bindings m

  let mem v (m, _) = (VarMap.mem v m)

  let reconstruct m = VarMap.bindings m |> construct

  let rm v (m, _) = VarMap.remove v m |> reconstruct

  let find v (m, _) = VarMap.find v m

  let filter f (m, _) = VarMap.filter f m |> reconstruct

  let rms vs t =
    let vs = VarSet.of_list vs in
    t |> filter (fun v _ -> VarSet.mem v vs |> not)

  let restrict vs t =
    let vs = VarSet.of_list vs in
    t |> filter (fun v _ -> VarSet.mem v vs)

  let leq (m1,_) (m2,_) =
    VarMap.for_all (fun v t ->
      VarMap.mem v m1 && TyScheme.leq (VarMap.find v m1) t
    ) m2

  let equiv env1 env2 = leq env1 env2 && leq env2 env1

  let pp fmt (m, _) =
    VarMap.bindings m
    |> List.iter (fun (v, ts) ->
      Format.fprintf fmt "%a: %a\n" Variable.pp v TyScheme.pp ts
    )

  let show = Format.asprintf "%a" pp

  let pp_filtered names fmt env =
    let env = filter (fun v _ -> List.mem (Variable.show v) names) env in
    pp fmt env

  let add v t e = assert (mem v e |> not) ; add v t e

  let tvars (_, s) = s

  let map f t =
    bindings t |> List.map (fun (v,t) -> (v,f t)) |> construct
end