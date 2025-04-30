open Parsing
open Variable
open Types.Base
open Types.Tvar

(* TODO: no more mono/poly var? *)

type e =
| Abstract of typ
| Const of Ast.const
| Var of Variable.t
| Atom of atom
| Tag of tag * t
| Lambda of (typ list) * Variable.t * t
| Ite of t * typ * t * t
| App of t * t
| Tuple of t list
| Cons of t * t
| Projection of Ast.projection * t
| RecordUpdate of t * string * t option
| Let of (typ list) * Variable.t * t * t
| TypeConstr of t * typ
| TypeCoerce of t * typ list
[@@deriving show]
and t = Ast.exprid * e
[@@deriving show]

let fixpoint_var = Variable.create_gen (Some "__builtin_fixpoint")
let fixpoint_typ =
  let a = TVar.mk_poly None |> TVar.typ in
  let b = TVar.mk_poly None |> TVar.typ in
  let f = mk_arrow a b in
  let c = TVar.mk_poly None |> TVar.typ in
  let fc = cap f c in
  let arg = mk_arrow f fc in
  mk_arrow arg fc
let initial_env =
  Env.construct [(fixpoint_var, fixpoint_typ)]

let map f =
  let rec aux (id,e) =
    let e =
      match e with
      | Abstract t -> Abstract t
      | Const c -> Const c
      | Var v -> Var v
      | Atom a -> Atom a
      | Tag (t,e) -> Tag (t, aux e)
      | Lambda (ta, v, e) -> Lambda (ta, v, aux e)
      | Ite (e, t, e1, e2) -> Ite (aux e, t, aux e1, aux e2)
      | App (e1, e2) -> App (aux e1, aux e2)
      | Tuple es -> Tuple (List.map aux es)
      | Cons (e1, e2) -> Cons (aux e1, aux e2)
      | Projection (p, e) -> Projection (p, aux e)
      | RecordUpdate (e, str, eo) -> RecordUpdate (aux e, str, Option.map aux eo)
      | Let (ta, v, e1, e2) -> Let (ta, v, aux e1, aux e2)
      | TypeConstr (e, t) -> TypeConstr (aux e, t)
      | TypeCoerce (e, ts) -> TypeCoerce (e, ts)
    in
    f (id,e)
  in
  aux

let fold f =
  let rec aux (id,e) =
    begin match e with
    | Abstract _ | Const _ | Var _ | Atom _ -> []
    | Tag (_, e) | Lambda (_,_, e) | Projection (_, e)
    | RecordUpdate (e, _, None) | TypeConstr (e,_) | TypeCoerce (e,_) -> [e]
    | Ite (e,_,e1,e2) -> [e ; e1 ; e2]
    | App (e1,e2) | Cons (e1,e2)
    | RecordUpdate (e1,_,Some e2) | Let (_,_,e1,e2) -> [e1 ; e2]
    | Tuple es -> es
    end
    |> List.map aux
    |> f (id,e)
  in
  aux

let fv' (_,e) accs =
  let acc = List.fold_left VarSet.union VarSet.empty accs in
  match e with
  | Abstract _ | Const _ | Atom _ | Tag _ | Ite _
  | App _ | Tuple _ | Cons _ | Projection _
  | RecordUpdate _ | TypeConstr _ | TypeCoerce _ -> acc
  | Var v -> VarSet.add v acc
  | Let (_, v, _, _) | Lambda (_, v, _) -> VarSet.remove v acc

let fv t = fold fv' t

(* Conversion *)

let rec type_of_pat pat =
  let open Ast in
  match pat with
  | PatType t -> t
  | PatVar _ -> any
  | PatTag (tag, p) -> mk_tag tag (type_of_pat p)
  | PatAnd (p1, p2) ->
    cap (type_of_pat p1) (type_of_pat p2)
  | PatOr (p1, p2) ->
    cup (type_of_pat p1) (type_of_pat p2)
  | PatTuple ps -> mk_tuple (List.map type_of_pat ps)
  | PatCons (p1, p2) ->
    let t2 = cap (type_of_pat p2) list_typ in
    mk_cons (type_of_pat p1) t2
  | PatRecord (fields, o) ->
    mk_record o (List.map (fun (str, p) -> (str, (false, type_of_pat p))) fields)
  | PatAssign _ -> any

let rec vars_of_pat pat =
  let open Ast in
  match pat with
  | PatType _ -> VarSet.empty
  | PatVar x when Variable.equals x dummy_pat_var -> VarSet.empty
  | PatVar x -> VarSet.singleton x
  | PatTag (_, p) -> vars_of_pat p
  | PatOr (p1, p2) ->
    VarSet.inter (vars_of_pat p1) (vars_of_pat p2)
  | PatTuple ps -> List.fold_left VarSet.union VarSet.empty (List.map vars_of_pat ps)
  | PatAnd (p1, p2) | PatCons (p1, p2) -> VarSet.union (vars_of_pat p1) (vars_of_pat p2)
  | PatRecord (fields, _) ->
    List.fold_left
      (fun acc (_, p) -> VarSet.union acc (vars_of_pat p))
      VarSet.empty fields
  | PatAssign (x,_) -> VarSet.singleton x

let rec def_of_var_pat pat v e =
  assert (Variable.equals v Ast.dummy_pat_var |> not) ;
  let open Ast in
  match pat with
  | PatVar v' when Variable.equals v v' -> e
  | PatVar _ -> assert false
  | PatTag (tag, p) ->
    def_of_var_pat p v (Ast.unique_exprid (), Projection (PiTag tag, e))
  | PatAnd (p1, p2) ->
    if vars_of_pat p1 |> VarSet.mem v
    then def_of_var_pat p1 v e
    else def_of_var_pat p2 v e
  | PatTuple ps ->
    let i = List.find_index (fun p -> vars_of_pat p |> VarSet.mem v) ps |> Option.get in
    let n = List.length ps in
    let p = List.nth ps i in
    def_of_var_pat p v (Ast.unique_exprid (), Projection (Pi (n,i), e))
  | PatCons (p1, p2) ->
    if vars_of_pat p1 |> VarSet.mem v
    then def_of_var_pat p1 v (Ast.unique_exprid (), Projection (Hd, e))
    else def_of_var_pat p2 v (Ast.unique_exprid (), Projection (Tl, e))
  | PatRecord (fields, _) ->
    let (str, p) =
      fields |> List.find (fun (_, p) -> vars_of_pat p |> VarSet.mem v)
    in
    def_of_var_pat p v (Ast.unique_exprid (), Projection (Field str, e))
  | PatOr (p1, p2) ->
    let case = Ite (e, type_of_pat p1,
      def_of_var_pat p1 v e, def_of_var_pat p2 v e) in
    (Ast.unique_exprid (), case)
  | PatAssign (v', c) when Variable.equals v v' -> (Ast.unique_exprid (), Const c)
  | PatAssign _ -> assert false
  | PatType _ -> assert false

let encode_pattern_matching id e pats =
  let x = Variable.create_let None in
  let ex : Ast.expr = (Ast.unique_exprid (), Var x) in
  let t = pats |> List.map fst |> List.map type_of_pat |> disj in
  let body_of_pat pat e' =
    let vars = vars_of_pat pat in
    let add_def acc v =
      let d = def_of_var_pat pat v ex in
      (Ast.unique_exprid (), Ast.Let (v, Ast.PNoAnnot, d, acc))
    in
    List.fold_left add_def e' (VarSet.elements vars)
  in
  let add_branch acc (t, e') =
    (Ast.unique_exprid (), Ast.Ite (ex, t, e', acc))
  in
  let pats = pats |> List.map (fun (pat, e') ->
    (type_of_pat pat, body_of_pat pat e')) |> List.rev in
  let body = match pats with
  | [] -> assert false 
  | (_, e')::pats -> List.fold_left add_branch e' pats
  in
  let def = (Ast.unique_exprid (), Ast.TypeConstr (e, t)) in
  (id, Ast.Let (x, Ast.PNoAnnot, def, body))

let encode_fixpoint id e =
  let x = Variable.create_let None in
  let ex : Ast.expr = (Ast.unique_exprid (), Var x) in
  let fix : Ast.expr = (Ast.unique_exprid (), Ast.Var fixpoint_var) in
  let app = (Ast.unique_exprid (), Ast.App (fix, ex)) in
  (id, Ast.Let (x, Ast.PNoAnnot, e, app))

let from_parser_ast t =
  let rec aux_e (id,e) =
    match e with
    | Ast.Abstract t -> Abstract t
    | Ast.Const c -> Const c
    | Ast.Var v -> Var v
    | Ast.Atom a -> Atom a
    | Ast.Tag (t, e) -> Tag (t, aux e)
    | Ast.Lambda (x, DNoAnnot, e) ->
      let tv = TVar.mk_mono ~infer:true (Variable.get_name x) |> TVar.typ in
      Lambda ([tv], x, aux e)
    | Ast.Lambda (x, DAnnot lst, e) -> Lambda (lst, x, aux e)
    | Ast.Fixpoint e -> encode_fixpoint id e |> aux_e
    | Ast.Ite (e,t,e1,e2) -> Ite (aux e, t, aux e1, aux e2)
    | Ast.App (e1,e2) -> App (aux e1, aux e2)
    | Ast.Let (x, PNoAnnot, e1, e2) -> Let ([], x, aux e1, aux e2)
    | Ast.Let (x, PAnnot ts, e1, e2) -> Let (ts, x, aux e1, aux e2)
    | Ast.Tuple es -> Tuple (List.map aux es)
    | Ast.Cons (e1, e2) -> Cons (aux e1, aux e2)
    | Ast.Projection (p, e) -> Projection (p, aux e)
    | Ast.RecordUpdate (e, lbl, eo) -> RecordUpdate (aux e, lbl, Option.map aux eo)
    | Ast.TypeConstr (e, t) -> TypeConstr (aux e, t)
    | Ast.TypeCoerce (e, ts) -> TypeCoerce (aux e, ts)
    | Ast.PatMatch (e, pats) -> encode_pattern_matching id e pats |> aux_e
  and aux t =
    let e = aux_e t in
    let (id, _) = t in
    (id,e)
  in
  aux t
