open Parsing.Variable
open Types
open Types.Base
open Types.Tvar

module type T = sig
  type t
  val fv : t -> TVarSet.t
  val leq : t -> t -> bool
  val substitute : Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
end  

module type Env = sig
  type t
  type ty
  val empty : t
  val is_empty : t -> bool
  val singleton : Variable.t -> ty -> t
  val construct : (Variable.t * ty) list -> t
  val add : Variable.t -> ty -> t -> t
  val domain : t -> Variable.t list
  val bindings : t -> (Variable.t * ty) list
  val mem : Variable.t -> t -> bool
  val find : Variable.t -> t -> ty
  val rm : Variable.t -> t -> t
  val rms : Variable.t list -> t -> t
  val restrict : Variable.t list -> t -> t
  val map : (ty -> ty) -> t -> t
  val filter : (Variable.t -> ty -> bool) -> t -> t
  val tvars : t -> TVarSet.t
  val substitute : Subst.t -> t -> t

  val equiv : t -> t -> bool
  val leq : t -> t -> bool

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_filtered : string list -> Format.formatter -> t -> unit
end

module Make(T:T) = struct
  type t = T.t VarMap.t * TVarSet.t

  let empty = (VarMap.empty, TVarSet.empty)
  let is_empty (m,_) =  VarMap.is_empty m
  let singleton v t = (VarMap.singleton v t, T.fv t)
  let construct lst = (VarMap.of_seq (List.to_seq lst),
    List.map snd lst |> List.map T.fv |> TVarSet.union_many)

  let add v t (m,s) = (VarMap.add v t m, TVarSet.union s (T.fv t))

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
      VarMap.mem v m1 && T.leq (VarMap.find v m1) t
    ) m2

  let equiv env1 env2 = leq env1 env2 && leq env2 env1

  let substitute s t =
    bindings t
    |> List.map (fun (x,t) -> (x,T.substitute s t))
    |> construct

  let pp fmt (m, _) =
    VarMap.bindings m
    |> List.iter (fun (v, ts) ->
      Format.fprintf fmt "%a: %a\n" Variable.pp v T.pp ts
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

module Env = Make(struct
  type t = TyScheme.t
  let fv = TyScheme.fv
  let leq = TyScheme.leq
  let substitute = TyScheme.substitute
  let pp = TyScheme.pp
end)

module REnv = struct
  include Make(struct
    type t = typ
    let fv = vars
    let leq = subtype
    let substitute = Subst.apply
    let pp = pp_typ
  end)

  let cap (m1, s1) (m2, s2) =
    (VarMap.union (fun _ t1 t2 ->
      Some (cap t1 t2)
      ) m1 m2,
    TVarSet.union s1 s2)
    
  let conj lst = List.fold_left cap empty lst
end

module PartitionTbl = struct
  type t = (Variable.t, typ list) Hashtbl.t

  let create () = Hashtbl.create 100

  let get_parts t v =
    match Hashtbl.find_opt t v with Some lst -> lst | None -> [any]
  
  let add_parts t v tys =
    let parts = get_parts t v in
    let parts = List.fold_left Types.Additions.refine_partition parts tys in
    Hashtbl.replace t v parts
end