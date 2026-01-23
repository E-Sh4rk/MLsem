open Mlsem_common
open Mlsem_types
open Ast
module SA = Mlsem_system.Ast

(* Pattern Matching *)

let constr_of_patconstr = function
  | PCTuple n -> SA.Tuple n
  | PCCons -> SA.Cons
  | PCRec (fields, opened) -> SA.Rec (fields, opened)
  | PCTag t -> SA.Tag t
  | PCEnum e -> SA.Enum e
  | PCCustom (c, _) -> SA.CCustom c

let proj_of_patconstr c i =
  match c,i with
  | PCTuple n, i when i < n -> SA.Pi (n,i)
  | PCCons, 0 -> SA.Hd
  | PCCons, 1 -> SA.Tl
  | PCRec (fields, _), i -> SA.PiField (List.nth fields i)
  | PCTag t, 0 -> SA.PiTag t
  | PCCustom (_, ps), i -> SA.PCustom (List.nth ps i)
  | _, _ -> invalid_arg "Wrong constructor arity."

let type_of_patconstr c args =
  let constr = constr_of_patconstr c in
  Mlsem_system.Checker.construct constr args

let rec type_of_pat pat =
  match pat with
  | PType t -> t
  | PVar _ -> Ty.any
  | PConstructor (c, args) ->
    let args = List.map type_of_pat args in
    type_of_patconstr c args
  | PAnd (p1, p2) -> Ty.cap (type_of_pat p1) (type_of_pat p2)
  | POr (p1, p2) -> Ty.cup (type_of_pat p1) (type_of_pat p2)
  | PAssign _ -> Ty.any

let rec vars_of_pat pat =
  match pat with
  | PType _ -> VarSet.empty
  | PVar (_,x) | PAssign (_,x,_) -> VarSet.singleton x
  | PConstructor (_, args) ->
    List.fold_left VarSet.union VarSet.empty (List.map vars_of_pat args)
  | POr (p1, p2) -> VarSet.inter (vars_of_pat p1) (vars_of_pat p2)
  | PAnd (p1, p2) -> VarSet.union (vars_of_pat p1) (vars_of_pat p2)

let rec def_of_var_pat pat v e =
  match pat with
  | PVar (tys,v') when Variable.equal v v' -> tys,e
  | PAssign (tys,v', ty) when Variable.equal v v' -> tys,(Eid.unique (), Value ty)
  | PVar _ | PAssign _ | PType _ -> assert false
  | PAnd (p1, p2) ->
    if vars_of_pat p1 |> VarSet.mem v
    then def_of_var_pat p1 v e
    else def_of_var_pat p2 v e
  | POr (p1, p2) ->
    let tys1, e1 = def_of_var_pat p1 v e in
    let tys2, e2 = def_of_var_pat p2 v e in
    let case = Ite (e, type_of_pat p1 |> GTy.mk, e1, e2) in
    tys1@tys2, (Eid.unique (), case)
  | PConstructor (c, ps) ->
    let i = List.find_index (fun p -> vars_of_pat p |> VarSet.mem v) ps |> Option.get in
    def_of_var_pat (List.nth ps i) v (Eid.unique (), Projection (proj_of_patconstr c i, e))

let encode_pattern_matching e pats =
  let x = MVariable.create Immut None in
  let ts = pats |> List.map fst |> List.map type_of_pat in
  let body_of_pat pat e =
    let add_def acc v =
      let tys,d = def_of_var_pat pat v (Eid.unique (), Var x) in
      (Eid.refresh (fst acc), Let (tys, v, d, acc))
    in
    List.fold_left add_def e (vars_of_pat pat |> VarSet.elements)
  in
  let add_branch acc (t, e) =
    let p1, p2 = Eid.loc (fst e), Eid.loc (fst acc) in
    (Eid.unique_with_pos (Position.join p1 p2), Ite ((Eid.unique (), Var x), GTy.mk t, e, acc))
  in
  let pats = pats |> List.map (fun (pat, e) ->
    (type_of_pat pat, body_of_pat pat e)) |> List.rev in
  let default_eid = Eid.refresh (fst e) in
  let () = Eid.set_show_notices default_eid false in
  let default = Eid.unique (), Constructor (SA.Normalize, [Eid.unique (), Var x]) in
  let default = default_eid, TypeCast (default, GTy.empty, CheckStatic) in
  let body = List.fold_left add_branch default pats in
  Let (ts, x, e, body)

let eliminate_pattern_matching e =
  let aux (id,e) =
    let e = match e with
    | PatMatch (e, pats) -> encode_pattern_matching e pats
    | e -> e
    in (id, e)
  in
  map aux e

(* Imperative constructs *)

let eliminate_if_while_break_return e =
  let aux (id,e) =
    match e with
    | Lambda (tys, ty, v, e) ->
      let block = Eid.unique (), Block (BFun, e) in
      id, Lambda (tys, ty, v, block)
    | If (e,t,e1,e2) ->
      let e2 = match e2 with None -> Eid.unique (), Exc | Some e2 -> e2 in
      let body = id, Ite (e, t, e1, e2) in
      Eid.unique (), Voidify body
    | While (e,t,e1) ->
      let body = id, Ite (e, t, e1, (Eid.unique (), Exc)) in
      let block = Eid.unique (), Block (BLoop, body) in
      let block = Eid.unique (), Voidify block in
      Eid.unique (), Loop block
    | Break -> id, Ret (BLoop, None)
    | Return e -> id, Ret (BFun, Some e)
    | e -> id, e
  in
  map aux e

(* Eliminate blocks *)

let has_eliminable_ret bid e =
  try
    let f = function
    | (_, Lambda _) | (_, LambdaRec _) | (_, Isolate _) | (_, Loop _) | (_, Alt _)
    | (_, App _) | (_, Constructor _) | (_, Operation _) | (_, Projection _)
    | (_, VarAssign _) -> false
    | (_, Block _) -> assert false
    | (_, Ret (bid', _)) when bid=bid' -> raise Exit
    | _ -> true
    in
    iter' f e ; false
  with Exit -> true

let rec try_elim_ret ~keep_ret bid e =
  let hole = Eid.dummy, Hole 0 in
  let fill e elt = fill_hole 0 elt e in
  let aux' e = try_elim_ret ~keep_ret:true bid e in
  let aux_noret' e = try_elim_ret ~keep_ret:false bid e in
  let rec aux (id,e) cont =
    let dup_cont cont = cont, cont |> Ast.map (fun (eid,t) -> (Eid.refresh eid, t)) in
    let cont' e = fill cont e in
    match e with
    (* Base cases *)
    | Hole _ | Void | Value _ | Var _ | Exc | Lambda _ | LambdaRec _ -> cont' (id,e)
    (* Do-not-traverse cases *)
    | Isolate e -> (id, Isolate (aux' e)) |> cont'
    | Constructor (c,es) -> (id, Constructor (c, List.map aux' es)) |> cont'
    | Alt (e1, e2) -> (id, Alt (aux' e1, aux' e2)) |> cont'
    | App (e1, e2) -> (id, App (aux' e1, aux' e2)) |> cont'
    | Operation (o,e) -> (id, Operation (o, aux' e)) |> cont'
    | Projection (p,e) -> (id, Projection (p, aux' e)) |> cont'
    | VarAssign (v,e) -> (id, VarAssign (v, aux' e)) |> cont'
    | Loop e -> (id, Loop (aux' e)) |> cont'
    (* Do-traverse cases *)
    | Voidify e ->
      (* Sound even when e is empty, because the continuation
         is always called at least once for non-ret expr *)
      (id, Voidify hole) |> cont' |> aux e
    | Declare (v, e) -> (id, Declare (v, aux e cont))
    | Let (tys, v, e1, e2) ->
      (id, Let (tys, v, hole, aux e2 cont)) |> aux e1
    | TypeCast (e, tau, c) ->
      (id, TypeCast (hole, tau, c)) |> cont' |> aux e
    | TypeCoerce (e, ty, c) ->
      (id, TypeCoerce (hole, ty, c)) |> cont' |> aux e
    | Try (e1, e2) when not (has_eliminable_ret bid e1) && not (has_eliminable_ret bid e2) ->
      (* Do not duplicate the continuation if unnecessary *)
      (id, Try (aux' e1, aux' e2)) |> cont'
    | Try (e1, e2) ->
      let cont1, cont2 = dup_cont cont in
      (id, Try (aux e1 cont1, aux e2 cont2))
    | Ite (e, tau, e1, e2) when not (has_eliminable_ret bid e1) && not (has_eliminable_ret bid e2) ->
      (* Do not duplicate the continuation if unnecessary *)
      (id, Ite (hole, tau, aux' e1, aux' e2)) |> cont' |> aux e
    | Ite (e, tau, e1, e2) ->
      let cont1, cont2 = dup_cont cont in
      (id, Ite (hole, tau, aux e1 cont1, aux e2 cont2)) |> aux e
    | Seq (e1,e2) -> (id, Seq (hole, aux e2 cont)) |> aux e1
    | Ret (bid', e) when bid'=bid && keep_ret ->
      id, Ret (bid', Option.map aux_noret' e)
    | Ret (bid', None) when bid'=bid -> id, Exc
    | Ret (bid', Some e) when bid'=bid -> aux_noret' e
    | Ret (bid', None) -> (id, Ret (bid', None)) |> cont'
    | Ret (bid', Some e) -> (id, Ret (bid', Some hole)) |> cont' |> aux e
    | PatMatch _ | If _ | While _  | Break | Return _ -> assert false
    | Block _ -> assert false
  in
  aux e hole

let has_ret ~incl_noarg bid e =
  try
    let f = function
    | (_, Lambda _) | (_, LambdaRec _) -> false
    | (_, Block _) -> assert false
    | (_, Ret (bid',None)) when incl_noarg && bid'=bid -> raise Exit
    | (_, Ret (bid',Some _)) when bid'=bid -> raise Exit
    | _ -> true
    in
    iter' f e ; false
  with Exit -> true

let rec elim_ret_args bid (id,e) =
  if has_ret ~incl_noarg:false bid (id,e)
  then
    let v = MVariable.create MVariable.Mut None in
    let body = Eid.refresh id, VarAssign (v, treat_rets bid v (id,e)) in
    let body = Eid.refresh id, Seq ((Eid.unique (), Voidify body), (Eid.unique (), Var v)) in
    Eid.refresh id, Declare (v, body)
  else id, e
and treat_rets bid v e =
  let f = function
  | (id,Lambda (tys, ty, v, e)) -> Some (id, Lambda (tys, ty, v, e))
  | (id,LambdaRec lst) -> Some (id, LambdaRec lst)
  | (_, Block _) -> assert false
  | (id, Ret (bid', Some e)) when bid'=bid ->
    let e = treat_rets bid v e in
    Some (id, Seq ((Eid.refresh id, VarAssign (v, e)), (Eid.refresh id, Ret (bid, None))))
  | _ -> None
  in
  map' f e

let elim_all_ret_noarg bid e =
  let f = function
  | (id,Lambda (tys, ty, v, e)) -> Some (id, Lambda (tys, ty, v, e))
  | (id,LambdaRec lst) -> Some (id, LambdaRec lst)
  | (_, Block _) -> assert false
  | (id, Ret (bid', None)) when bid'=bid -> Some (id, Exc)
  | _ -> None
  in
  map' f e

let clean_unreachable e =
  let rec ends_with_exc e =
    match snd e with
    | Exc -> true
    | Seq (_, e2) -> ends_with_exc e2
    | _ -> false
  in
  let f = function
  | (_, Block _) -> assert false
  | (_, Seq (e, _)) when ends_with_exc e -> e
  | e -> e
  in
  map f e

let eliminate_blocks e =
  let aux (id,e) =
    match e with
    | Block (bid, e) ->
      try_elim_ret ~keep_ret:false bid e |> elim_ret_args bid |> elim_all_ret_noarg bid
    | e -> id, e
  in
  map aux e |> clean_unreachable

(* Main *)

let eliminate_cf t =
  let rec aux (id, e) =
    let e = match e with
    | Hole n -> MAst.Hole n
    | Void -> MAst.Void
    | Voidify e -> MAst.Voidify (aux e)
    | Isolate e -> aux e |> snd
    | Value t -> MAst.Value t
    | Var v -> MAst.Var v
    | Constructor (c, es) -> MAst.Constructor (c, List.map aux es)
    | Lambda (tys, ty, x, e) -> MAst.Lambda (tys, ty, x, aux e)
    | LambdaRec lst ->
      let aux (ty,x,e) = (ty, x, aux e) in
      MAst.LambdaRec (List.map aux lst)
    | Ite (e,t,e1,e2) -> MAst.Ite (aux e, t, aux e1, aux e2)
    | Try (e1,e2) -> MAst.Try (aux e1, aux e2)
    | App (e1,e2) -> MAst.App (aux e1, aux e2)
    | Operation (o, e) -> MAst.Operation (o, aux e)
    | Projection (p, e) -> MAst.Projection (p, aux e)
    | Declare (x, e) -> MAst.Declare (x, aux e)
    | Let (tys, x, e1, e2) -> MAst.Let (tys, x, aux e1, aux e2)
    | TypeCast (e, ty, c) -> MAst.TypeCast (aux e, ty, c)
    | TypeCoerce (e, ty, c) -> MAst.TypeCoerce (aux e, ty, c)
    | VarAssign (v, e) -> MAst.VarAssign (v, aux e)
    | Loop e -> MAst.Loop (aux e)
    | Seq (e1, e2) -> MAst.Seq (aux e1, aux e2)
    | Alt (e1, e2) -> MAst.Alt (aux e1, aux e2)
    | Exc -> Exc
    | PatMatch _ | If _ | While _ | Break | Return _ | Block _ -> assert false
    | Ret _ -> invalid_arg "Expression contains an orphan ret expression."
    in
    (id, e)
  in
  aux t

let eliminate_cf t =
  t
  |> eliminate_pattern_matching
  |> eliminate_if_while_break_return
  |> eliminate_blocks
  |> eliminate_cf

let transform t =
  t
  |> eliminate_cf
  |> Optimize.optimize_dataflow
  |> MAst.to_system_ast
