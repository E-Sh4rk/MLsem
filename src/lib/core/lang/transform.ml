open Mlsem_common
open Mlsem_types
open Ast
module SA = Mlsem_system.Ast

(* Pattern Matching *)

let constr_of_patconstr = function
  | PCTuple n -> SA.Tuple n
  | PCCons -> SA.Cons
  | PCRec (fields, opened) -> SA.Rec (List.map (fun f -> (f, false)) fields, opened)
  | PCTag t -> SA.Tag t
  | PCEnum e -> SA.Enum e
  | PCCustom (c, _) -> SA.CCustom c

let proj_of_patconstr c i =
  match c,i with
  | PCTuple n, i when i < n -> SA.Pi (n,i)
  | PCCons, 0 -> SA.Hd
  | PCCons, 1 -> SA.Tl
  | PCRec (fields, _), i -> SA.Field (List.nth fields i)
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
  | PVar x | PAssign (x,_) -> VarSet.singleton x
  | PConstructor (_, args) ->
    List.fold_left VarSet.union VarSet.empty (List.map vars_of_pat args)
  | POr (p1, p2) -> VarSet.inter (vars_of_pat p1) (vars_of_pat p2)
  | PAnd (p1, p2) -> VarSet.union (vars_of_pat p1) (vars_of_pat p2)

let rec def_of_var_pat pat v e =
  match pat with
  | PVar v' when Variable.equals v v' -> e
  | PAssign (v', ty) when Variable.equals v v' -> (Eid.unique (), Value ty)
  | PVar _ | PAssign _ | PType _ -> assert false
  | PAnd (p1, p2) ->
    if vars_of_pat p1 |> VarSet.mem v
    then def_of_var_pat p1 v e
    else def_of_var_pat p2 v e
  | POr (p1, p2) ->
    let case = Ite (e, type_of_pat p1, def_of_var_pat p1 v e, def_of_var_pat p2 v e) in
    (Eid.unique (), case)
  | PConstructor (c, ps) ->
    let i = List.find_index (fun p -> vars_of_pat p |> VarSet.mem v) ps |> Option.get in
    def_of_var_pat (List.nth ps i) v (Eid.unique (), Projection (proj_of_patconstr c i, e))

let encode_pattern_matching e pats =
  let x = Variable.create_gen None in
  let ts = pats |> List.map fst |> List.map type_of_pat in
  let body_of_pat pat e =
    let add_def acc v =
      let d = def_of_var_pat pat v (Eid.unique (), Var x) in
      (Eid.refresh (fst acc), Let ([], v, d, acc))
    in
    List.fold_left add_def e (vars_of_pat pat |> VarSet.elements)
  in
  let add_branch acc (t, e) =
    let p1, p2 = Eid.loc (fst e), Eid.loc (fst acc) in
    (Eid.unique_with_pos (Position.join p1 p2), Ite ((Eid.unique (), Var x), t, e, acc))
  in
  let pats = pats |> List.map (fun (pat, e) ->
    (type_of_pat pat, body_of_pat pat e)) |> List.rev in
  let body = match pats with
  | [] -> assert false
  | (_, e')::pats -> List.fold_left add_branch e' pats
  in
  let e = (Eid.refresh (fst e), TypeCast (e, Ty.disj ts)) in
  Let (ts, x, e, body)

let eliminate_pattern_matching e =
  let aux (id,e) =
    let e = match e with
    | PatMatch (e, pats) -> encode_pattern_matching e pats
    | e -> e
    in (id, e)
  in
  map aux e

(* If and While *)

let eliminate_if_while e =
  let aux (id,e) =
    let e = match e with
    | If (e,t,e1,None) -> VoidConditional (false, e, t, e1, (Eid.unique (), Void))
    | If (e,t,e1,Some e2) -> VoidConditional (false, e, t, e1, e2)
    | While (e,t,e1) -> VoidConditional (true, e, t, e1, (Eid.unique (), Void))
    | e -> e
    in (id, e)
  in
  map aux e

(* Break *)

let has_break e =
  try
    let f = function
    | (_, Lambda _) | (_, LambdaRec _) -> false
    | (_, VoidConditional (true, _, _, _, _)) -> false
    | (_, App _) | (_, Constructor (_, _::_::_)) -> false
    | (_, Isolate _) -> false
    | (_, Break) -> raise Exit
    | _ -> true
    in
    iter' f e ; false
  with Exit -> true

let rec eliminate_break e =
  let hole = Eid.dummy, Hole 0 in
  let fill e elt = fill_hole 0 elt e in
  let rec aux (id,e) cont =
    let cont' e = fill cont e in
    match e with
    | Void | Value _ | Var _ | Constructor (_,[]) | Exc -> cont' (id,e)
    | Voidify e -> (id, Voidify hole) |> cont' |> aux e
    | Isolate e -> (id, Isolate (eliminate_inner_break e)) |> cont'
    | Declare (tys, v, e) -> (id, Declare (tys, v, aux e cont))
    | Let (tys, v, e1, e2) ->
      (id, Let (tys, v, hole, aux e2 cont)) |> aux e1
    | Projection (p, e) ->
      (id, Projection (p, hole)) |> cont' |> aux e
    | TypeCast (e, tau) ->
      (id, TypeCast (hole, tau)) |> cont' |> aux e
    | TypeCoerce (e, ty, c) ->
      (id, TypeCoerce (hole, ty, c)) |> cont' |> aux e
    | VarAssign (v, e) ->
      (id, VarAssign (v, hole)) |> cont' |> aux e
    | Constructor (c, [e]) ->
      (id, Constructor (c, [hole])) |> cont' |> aux e
    | Constructor (c, es) ->
      (id, Constructor (c, List.map eliminate_inner_break es)) |> cont'
    | Try es when not (List.exists has_break es) ->
      (* Do not duplicate the continuation if unnecessary *)
      (id, Try (List.map eliminate_inner_break es)) |> cont'
    | Try (es) -> (id, Try (es |> List.map (fun e -> aux e cont)))
    | App (e1, e2) ->
      let e1, e2 = eliminate_inner_break e1, eliminate_inner_break e2 in
      (id, App (e1, e2)) |> cont'
    | Ite (e, tau, e1, e2) when not (has_break e1) && not (has_break e2) ->
      (* Do not duplicate the continuation if unnecessary *)
      let e1, e2 = eliminate_inner_break e1, eliminate_inner_break e2 in
      (id, Ite (hole, tau, e1, e2)) |> cont' |> aux e
    | Ite (e, tau, e1, e2) ->
      (id, Ite (hole, tau, aux e1 cont, aux e2 cont)) |> aux e
    | Lambda (tys, ty, x, e) -> (id, Lambda (tys, ty, x, eliminate_break e)) |> cont'
    | LambdaRec lst ->
      (id, LambdaRec (List.map (fun (ty,v,e) -> ty,v,eliminate_break e) lst)) |> cont'
    | VoidConditional (true, e, tau, e1, e2) ->
      (id, VoidConditional (false, e, tau, eliminate_break e1, eliminate_break e2)) |> cont'
    | VoidConditional (false, e, tau, e1, e2) when not (has_break e1) && not (has_break e2) ->
      (* Do not duplicate the continuation if unnecessary *)
      let e1, e2 = eliminate_inner_break e1, eliminate_inner_break e2 in
      (id, VoidConditional (false, hole, tau, e1, e2)) |> cont' |> aux e
    | VoidConditional (false, e, tau, e1, e2) ->
      let e1, e2 = (Eid.unique (), Voidify e1), (Eid.unique (), Voidify e2) in
      (id, Ite (hole, tau, aux e1 cont, aux e2 cont)) |> aux e
    | Seq (e1,e2) -> (id, Seq (hole, aux e2 cont)) |> aux e1
    | Return e -> (id, Return hole) |> cont' |> aux e
    | Break -> id, Value (GTy.mk Ty.empty)
    | PatMatch _ | If _ | While _ -> assert false
    | Hole _ -> invalid_arg "Expression should not contain a hole."
  in
  aux e hole
and eliminate_inner_break e =
  let f = function
  | (id,Lambda (tys, ty, v, e)) -> Some (id, Lambda (tys, ty, v, eliminate_break e))
  | (id,LambdaRec lst) ->
    Some (id, LambdaRec (lst |> List.map (fun (ty,v,e) -> ty,v,eliminate_break e)))
  | (id,VoidConditional (true, e, t, e1, e2)) ->
    Some (id, VoidConditional (true, eliminate_inner_break e, t, eliminate_break e1, eliminate_break e2))
  | _ -> None
  in
  map' f e

(* Return *)

let has_return e =
  try
    let f = function
    | (_, Lambda _) | (_, LambdaRec _) -> false
    | (_, App _) | (_, Constructor (_, _::_::_)) -> false
    | (_, Isolate _) -> false
    | (_, Return _) -> raise Exit
    | _ -> true
    in
    iter' f e ; false
  with Exit -> true

let rec eliminate_return e =
  let hole = Eid.dummy, Hole 0 in
  let fill e elt = fill_hole 0 elt e in
  let rec aux (id,e) cont =
    let cont' e = fill cont e in
    match e with
    | Void | Value _ | Var _ | Constructor (_,[]) | Break | Exc -> cont' (id,e)
    | Voidify e -> (id, Voidify hole) |> cont' |> aux e
    | Isolate e -> (id, Isolate (eliminate_inner_return e)) |> cont'
    | Declare (tys, v, e) -> (id, Declare (tys, v, aux e cont))
    | Let (tys, v, e1, e2) ->
      (id, Let (tys, v, hole, aux e2 cont)) |> aux e1
    | Projection (p, e) ->
      (id, Projection (p, hole)) |> cont' |> aux e
    | TypeCast (e, tau) ->
      (id, TypeCast (hole, tau)) |> cont' |> aux e
    | TypeCoerce (e, ty, c) ->
      (id, TypeCoerce (hole, ty, c)) |> cont' |> aux e
    | VarAssign (v, e) ->
      (id, VarAssign (v, hole)) |> cont' |> aux e
    | Constructor (c, [e]) ->
      (id, Constructor (c, [hole])) |> cont' |> aux e
    | Constructor (c, es) ->
      (id, Constructor (c, List.map eliminate_inner_return es)) |> cont'
    | Try es when not (List.exists has_return es) ->
      (* Do not duplicate the continuation if unnecessary *)
      (id, Try (List.map eliminate_inner_return es)) |> cont'
    | Try (es) -> (id, Try (es |> List.map (fun e -> aux e cont)))
    | App (e1, e2) ->
      let e1, e2 = eliminate_inner_return e1, eliminate_inner_return e2 in
      (id, App (e1, e2)) |> cont'
    | Ite (e, tau, e1, e2) when not (has_return e1) && not (has_return e2) ->
      (* Do not duplicate the continuation if unnecessary *)
      let e1, e2 = eliminate_inner_return e1, eliminate_inner_return e2 in
      (id, Ite (hole, tau, e1, e2)) |> cont' |> aux e
    | Ite (e, tau, e1, e2) ->
      (id, Ite (hole, tau, aux e1 cont, aux e2 cont)) |> aux e
    | Lambda (tys, ty, x, e) -> (id, Lambda (tys, ty, x, eliminate_return e)) |> cont'
    | LambdaRec lst ->
      (id, LambdaRec (List.map (fun (ty,v,e) -> ty,v,eliminate_return e) lst)) |> cont'
    | VoidConditional (false, e, tau, e1, e2) when not (has_return e1) && not (has_return e2) ->
      (* Do not duplicate the continuation if unnecessary *)
      let e1, e2 = eliminate_inner_return e1, eliminate_inner_return e2 in
      (id, VoidConditional (false, hole, tau, e1, e2)) |> cont' |> aux e
    | VoidConditional (false, e, tau, e1, e2) ->
      let e1, e2 = (Eid.unique (), Voidify e1), (Eid.unique (), Voidify e2) in
      (id, Ite (hole, tau, aux e1 cont, aux e2 cont)) |> aux e
    | Seq (e1,e2) -> (id, Seq (hole, aux e2 cont)) |> aux e1
    | Return e -> e
    | PatMatch _ | If _ | While _ | VoidConditional (true, _, _, _, _) -> assert false
    | Hole _ -> invalid_arg "Expression should not contain a hole."
  in
  aux e hole
and eliminate_inner_return e =
  let f = function
  | (id,Lambda (tys, ty, v, e)) -> Some (id, Lambda (tys, ty, v, eliminate_return e))
  | (id,LambdaRec lst) ->
    Some (id, LambdaRec (lst |> List.map (fun (ty,v,e) -> ty,v,eliminate_return e)))
  | _ -> None
  in
  map' f e

(* Unify remaining returns *)

let has_return e =
  try
    let f = function
    | (_, Lambda _) | (_, LambdaRec _) -> false
    | (_, Return _) -> raise Exit
    | _ -> true
    in
    iter' f e ; false
  with Exit -> true

let rec unify_returns e =
  if has_return e
  then
    let v = MVariable.create_let MVariable.Mut (Some "res") in
    let body = Eid.unique (), VarAssign (v, treat_returns v e) in
    let body = Eid.unique (), Seq ((Eid.unique (), Voidify body), (Eid.unique (), Var v)) in
    Eid.unique (), Declare ([], v, body)
  else unify_inner_returns e
and unify_inner_returns e =
  let f = function
  | (id,Lambda (tys, ty, v, e)) -> Some (id, Lambda (tys, ty, v, unify_returns e))
  | _ -> None
  in
  map' f e
and treat_returns v e =
  let f = function
  | (id,Lambda (tys, ty, v, e)) -> Some (id, Lambda (tys, ty, v, e))
  | (id, Return e) ->
    let e = treat_returns v e in
    let ret = Eid.unique (), Return ((Eid.unique (), Var v)) in
    Some (id, Seq ((Eid.unique (), VarAssign (v, e)), ret))
  | _ -> None
  in
  map' f e

(* Main *)

let transform t =
  let rec aux (id, e) =
    let e = match e with
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
    | Try es -> MAst.Try (List.map aux es)
    | App (e1,e2) -> MAst.App (aux e1, aux e2)
    | Projection (p, e) -> MAst.Projection (p, aux e)
    | Declare (tys, x, e) -> MAst.Declare (tys, x, aux e)
    | Let (tys, x, e1, e2) -> MAst.Let (tys, x, aux e1, aux e2)
    | TypeCast (e, ty) -> MAst.TypeCast (aux e, ty)
    | TypeCoerce (e, ty, c) -> MAst.TypeCoerce (aux e, ty, c)
    | VarAssign (v, e) -> MAst.VarAssign (v, aux e)
    | VoidConditional (_,e,t,e1,e2) ->
      let e1, e2 = (Eid.unique (), Voidify e1) |> aux, (Eid.unique (), Voidify e2) |> aux in
      MAst.Ite (aux e, t, e1, e2)
    | Seq (e1, e2) -> MAst.Seq (aux e1, aux e2)
    | Break | Return _ | Exc -> Exc (* Fallback for breaks, exc, and unified returns *)
    | PatMatch _ | If _ | While _ -> assert false
    | Hole _ -> invalid_arg "Expression should not contain a hole."
    in
    (id, e)
  in
  aux t

let eliminate_cf t =
  t
  |> eliminate_pattern_matching
  |> eliminate_if_while
  |> eliminate_break
  |> eliminate_return
  |> unify_returns
  |> transform

let transform t =
  t
  |> eliminate_cf
  |> MAst.to_system_ast
