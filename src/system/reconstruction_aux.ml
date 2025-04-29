open Types.Base
open Types.Tvar
open Types.Additions
open Parsing.Variable
open Msc
open Annotations
open Utils
open Algorithmic

module Make () = struct

(* ====================================== *)
(* ============== CACHING =============== *)
(* ====================================== *)

let fv_def_htbl = Hashtbl.create 100
let init =
  let rec init_a v a =
    Hashtbl.add fv_def_htbl v (Msc.fv_a a) ;
    match a with
    | Lambda (_,_,e) -> init e
    | _ -> ()
  and init e =
    match e with
    | Var _ -> ()
    | Bind (v, a, e) ->
      init_a v a ; init e
  in
  init

let fv_def v = Hashtbl.find fv_def_htbl v

type icache = { context: Env.t ; pannot: PartialAnnot.a ; res: FullAnnot.a_cached }

let inter_cache = Hashtbl.create 100

let add_to_inter_cache x env pannot res =
  if Settings.enable_caching_reconstruction () then
    let fv = fv_def x in
    let env = Env.restrict (VarSet.elements fv) env in
    Hashtbl.add inter_cache x { context=env; pannot=pannot; res=res }

let get_inter_cache x env pannot =
  if Settings.enable_caching_reconstruction () then
    let fv = fv_def x in
    let env = Env.restrict (VarSet.elements fv) env in
    let caches = Hashtbl.find_all inter_cache x in
    caches |> List.find_opt
      (fun ic -> PartialAnnot.equals_a pannot ic.pannot && Env.equiv env ic.context)
    |> Option.map (fun ic -> ic.res)
  else None

let clear_cache () =
  Hashtbl.clear inter_cache ;
  Hashtbl.clear fv_def_htbl

(* ====================================== *)
(* =========== SIMPLIFICATION =========== *)
(* ====================================== *)

let simplify_tallying res sols =
  let is_better_sol s1 s2 =
    let t1 = Subst.apply s1 res in
    let t2 = Subst.apply s2 res in
    subtype_poly t1 t2
  in
  let sols = sols |> List.map (fun sol ->
    (* Basic cleaning *)
    let t = Subst.apply sol res in
    let clean = clean_type_subst ~pos:empty ~neg:any t in
    Subst.compose clean sol
  ) in
  (* Remove weaker solutions *)
  let sols = keep_only_minimal is_better_sol sols in
  sols

(* ====================================== *)
(* ============= POLY INFER ============= *)
(* ====================================== *)

let tallying_nonempty constr =
  match tallying constr with
  | [] -> assert false
  | sols -> sols

let tallying_one constr =
  match tallying constr with
  | [] -> assert false
  | sol::_ -> sol

let rec infer_poly_a vardef tenv env pannot_a a =
  let open PartialAnnot in
  let open FullAnnot in
  match get_inter_cache vardef env pannot_a with
  | Some res -> res
  | None ->
    let vartype v = Env.find v env in
    let annot_a = match a, pannot_a with
    | a, InterA (b1, b2, (_,tf,_)) ->
      assert (b1 = [] && b2 <> [] && tf) ;
      let lst = b2 |> List.map
        (fun pannot_a -> infer_poly_a vardef tenv env pannot_a a)
      in InterA lst
    | Alias _, TypA -> AliasA
    | Const _, TypA -> ConstA
    | Let _, TypA -> LetA
    | Abstract _, TypA -> AbstractA
    | Atom _, TypA -> AtomA
    | Tag _, TypA -> TagA
    | Tuple vs, TypA ->
      let rs = vs |> List.map (fun v -> refresh (vartype v |> vars_poly)) in
      TupleA rs
    | Cons (v1, v2), TypA ->
      let r1 = refresh (vartype v1 |> vars_poly) in
      let res = tallying_nonempty [(vartype v2, list_typ)] in
      let res = simplify_tallying list_typ res in
      ConsA (r1, res)
    | Projection (p, v), TypA ->
      let t = vartype v in
      let alpha = TVar.mk_poly None in
      let s = Algorithmic.domain_of_proj p (TVar.typ alpha) in
      log ~level:4 "@.Tallying for %a: %a <= %a@."
        Variable.pp vardef pp_typ t pp_typ s ;
      let res = tallying_nonempty [(t, s)] in
      let res = simplify_tallying (TVar.typ alpha) res in
      ProjA res
    | RecordUpdate (v, _, None), TypA ->
      let res = tallying_nonempty [(vartype v, record_any)] in
      let res = simplify_tallying record_any res in
      RecordUpdateA (res, None)
    | RecordUpdate (v, _, Some v2), TypA ->
      let res = tallying_nonempty [(vartype v, record_any)] in
      let res = simplify_tallying record_any res in
      let r = refresh (vartype v2 |> vars_poly) in
      RecordUpdateA (res, Some r)
    | TypeConstr (v, s), TypA ->
      ConstrA [tallying_one [(vartype v, disj s)]]
    | TypeCoercion (v, s), TypA ->
      begin match subtypes_expand ~max_exp:1 (vartype v) s with
      | None -> assert false
      | Some inst -> CoercA inst
      end
    | App (v1, v2), TypA ->
      let t1 = vartype v1 in
      let t2 = vartype v2 in
      let r1 = refresh (vars_poly t1) in
      let r2 = refresh (vars_poly t2) in
      let t1 = Subst.apply r1 t1 in
      let t2 = Subst.apply r2 t2 in
      let alpha = TVar.mk_poly None in
      let arrow_type = mk_arrow t2 (TVar.typ alpha) in
      log ~level:4 "@.Tallying for %a: %a <= %a@."
        Variable.pp vardef pp_typ t1 pp_typ arrow_type ;
      let res = tallying [(t1, arrow_type)] in
      assert (List.length res > 0) ;
      let res = simplify_tallying (TVar.typ alpha) res in
      let (s1, s2) = res |> List.map (fun s ->
        (Subst.compose_restr s r1, Subst.compose_restr s r2)
      ) |> List.split in
      AppA (s1, s2)
    | Ite (v, s, _, _), TypA ->
      begin match tallying [(vartype v, empty)] with
      | sol::_ -> EmptyA [sol]
      | [] ->
        begin match tallying [(vartype v, s)] with
        | sol::_ -> ThenA [sol]
        | [] -> ElseA [tallying_one [(vartype v, neg s)]]
        end
      end
    | Lambda (_, v, e), PartialAnnot.LambdaA (s, pannot) ->
      let env = Env.add v s env in
      let annot = infer_poly tenv env pannot e in
      LambdaA (s, annot)
    | _, _ ->  assert false
    in
    let annot_a = (annot_a, init_cache ()) in
    add_to_inter_cache vardef env pannot_a annot_a ;
    annot_a

and infer_poly tenv env pannot e =
  let open PartialAnnot in
  let open FullAnnot in
  let vartype v = Env.find v env in
  let annot = match e, pannot with
  | e, Inter (b1, b2, (_,tf,_)) ->
    assert (b1 = [] && b2 <> [] && tf) ;
    let lst = b2 |> List.map (fun pannot -> infer_poly tenv env pannot e)
    in Inter lst
  | Var v, Typ ->
    let r = refresh (vartype v |> vars_poly) in
    BVar r
  | Bind (_, _, e), PartialAnnot.Skip pannot ->
    let annot = infer_poly tenv env pannot e in
    Skip annot
  | Bind (v, a, e), PartialAnnot.Keep (pannot_a, (ex,d,u)) ->
    assert (d <> [] && ex = []) ;
    let annot_a = infer_poly_a v tenv env pannot_a a in
    let t = typeof_a_nofail v tenv env annot_a a in
    assert (subtype any (u@(List.map fst d) |> disj)) ;
    let branches = d |> List.map (fun (si, pannot) ->
        let t = cap t si in
        let env = Env.add v t env in
        let annot = infer_poly tenv env pannot e in
        (si, annot)
      )
    in
    let inst = u |> List.map (fun u -> tallying_one [(t, neg u)]) in
    Keep (annot_a, branches, inst)
  | _, _ ->  assert false
  in
  (annot, init_cache ())

end