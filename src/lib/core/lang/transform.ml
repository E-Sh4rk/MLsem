open Common
open Types
open Ast
module SA = System.Ast

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
  System.Checker.construct constr args

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
    | If (e,t,e1,None) -> Conditional (false, e, t, e1, (Eid.unique (), Void))
    | If (e,t,e1,Some e2) -> Conditional (false, e, t, e1, e2)
    | While (e,t,e1) -> Conditional (true, e, t, e1, (Eid.unique (), Void))
    | e -> e
    in (id, e)
  in
  map aux e

(* Break *)

let has_break e =
  try
    let f = function
    | (_, Lambda _) -> false
    | (_, Conditional (true, _, _, _, _)) -> false
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
    | Void | Value _ | Var _ | Constructor (_,[]) -> cont' (id,e)
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
      (id, LambdaRec (List.map (fun (ty,v,e) -> ty,v,eliminate_inner_break e) lst)) |> cont'
    | Conditional (true, e, tau, e1, e2) ->
      (id, Conditional (true, e, tau, eliminate_break e1, eliminate_break e2)) |> cont'
    | Conditional (false, e, tau, e1, e2) when not (has_break e1) && not (has_break e2) ->
      (* Do not duplicate the continuation if unnecessary *)
      let e1, e2 = eliminate_inner_break e1, eliminate_inner_break e2 in
      (id, Conditional (false, hole, tau, e1, e2)) |> cont' |> aux e
    | Conditional (false, e, tau, e1, e2) ->
      let e1 = Eid.unique (), Seq (e1, (Eid.unique (), Void)) in
      let e2 = Eid.unique (), Seq (e2, (Eid.unique (), Void)) in
      (id, Ite (hole, tau, aux e1 cont, aux e2 cont)) |> aux e
    | Seq (e1,e2) -> (id, Seq (hole, aux e2 cont)) |> aux e1
    | Return e -> (id, Return hole) |> cont' |> aux e
    | Break -> id, Void
    | PatMatch _ | If _ | While _ -> assert false
    | Hole _ -> invalid_arg "Expression should not contain a hole."
  in
  aux e hole
and eliminate_inner_break e =
  let f = function
  | (id,Lambda (tys, ty, v, e)) -> Some (id, Lambda (tys, ty, v, eliminate_break e))
  | (id,Conditional (true, e, t, e1, e2)) ->
    Some (id, Conditional (true, eliminate_inner_break e, t, eliminate_break e1, eliminate_break e2))
  | _ -> None
  in
  map' f e

(* Return *)

let has_return e =
  try
    let f = function
    | (_, Lambda _) -> false
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
    | Void | Value _ | Var _ | Constructor (_,[]) | Break -> cont' (id,e)
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
      (id, LambdaRec (List.map (fun (ty,v,e) -> ty,v,eliminate_inner_return e) lst)) |> cont'
    | Conditional (b, e, tau, e1, e2) when not (has_return e1) && not (has_return e2) ->
      (* Do not duplicate the continuation if unnecessary *)
      let e1, e2 = eliminate_inner_return e1, eliminate_inner_return e2 in
      (id, Conditional (b, hole, tau, e1, e2)) |> cont' |> aux e
    | Conditional (_, e, tau, e1, e2) ->
      let e1 = Eid.unique (), Seq (e1, (Eid.unique (), Void)) in
      let e2 = Eid.unique (), Seq (e2, (Eid.unique (), Void)) in
      (id, Ite (hole, tau, aux e1 cont, aux e2 cont)) |> aux e
    | Seq (e1,e2) -> (id, Seq (hole, aux e2 cont)) |> aux e1
    | Return e -> e
    | PatMatch _ | If _ | While _ -> assert false
    | Hole _ -> invalid_arg "Expression should not contain a hole."
  in
  aux e hole
and eliminate_inner_return e =
  let f = function
  | (id,Lambda (tys, ty, v, e)) -> Some (id, Lambda (tys, ty, v, eliminate_return e))
  | _ -> None
  in
  map' f e

(* Main *)

(* TODO: Fallback for return statements *)
let transform t =
  let rec aux (id, e) =
    let e = match e with
    | Void -> SA.Value (GTy.mk !System.Config.void_ty)
    | Value t -> SA.Value t
    | Var v ->
      if MVariable.is_mutable v then
        SA.App ((Eid.unique (), SA.Value (MVariable.ref_get v |> GTy.mk)),
                (Eid.unique (), SA.Var v))
      else
        SA.Var v
    | Constructor (c, es) -> SA.Constructor (c, List.map aux es)
    | Lambda (tys, ty, x, e) ->
      let x' = MVariable.create_let (MVariable.kind x) (Variable.get_name x) in
      Variable.get_location x |> Variable.attach_location x' ;
      let body =
        Eid.refresh (fst e),
        Let (tys, x', (Eid.unique (), Var x), rename_fv x x' e)
      in
      SA.Lambda (ty, x, aux body)
    | LambdaRec lst ->
      let aux (ty,x,e) = (ty, x, aux e) in
      SA.LambdaRec (List.map aux lst)
    | Ite (e,t,e1,e2) -> SA.Ite (aux e, t, aux e1, aux e2)
    | App (e1,e2) -> SA.App (aux e1, aux e2)
    | Projection (p, e) -> SA.Projection (p, aux e)
    | Let (tys, x, e1, e2) ->
      let tys, def = if MVariable.is_mutable x
        then [], (Eid.unique (), SA.App (
          (Eid.unique (), SA.Value (MVariable.ref_cons x |> GTy.mk)),
          aux e1))
        else tys, aux e1
      in
      SA.Let (tys, x, def, aux e2)
    | TypeCast (e, ty) -> SA.TypeCast (aux e, ty)
    | TypeCoerce (e, ty, c) -> SA.TypeCoerce (aux e, ty, c)
    | VarAssign (v, e) when MVariable.is_mutable v -> SA.App (
        (Eid.unique (), SA.Value (MVariable.ref_assign v |> GTy.mk)),
        (Eid.unique (), SA.Constructor (SA.Tuple 2,[
            (Eid.unique (), SA.Var v) ; aux e
        ]))
      )
    | VarAssign _ -> invalid_arg "Cannot assign to an immutable variable."
    | Conditional (_,e,t,e1,(_,Void)) -> SA.Conditional (aux e, t, aux e1)
    | Conditional (_,e,t,(_,Void),e2) -> SA.Conditional (aux e, Ty.neg t, aux e2)
    | Conditional (b, e,t,e1,e2) ->
      let x = Variable.create_gen None in
      let body = Eid.unique (), Seq (
        (Eid.unique (), Conditional (b,(Eid.unique (), Var x),t,e1,(Eid.unique (), Void))),
        (Eid.unique (), Conditional (b,(Eid.unique (), Var x),t,(Eid.unique (), Void),e2))
      ) in
      Let ([], x, aux e, aux body)
    | Seq (e1, e2) -> Let ([], Variable.create_gen None, aux e1, aux e2)
    | Break -> SA.Value (GTy.mk Ty.empty) (* Fallback for breaks *)
    | PatMatch _ | If _ | While _ -> assert false
    | Return _ -> invalid_arg "Expression has return statements that cannot be eliminated."
    | Hole _ -> invalid_arg "Expression should not contain a hole."
    in
    (id, e)
  in
  aux t

let transform t =
  t
  |> eliminate_pattern_matching
  |> eliminate_if_while
  |> eliminate_break
  |> eliminate_return
  |> transform
