open Parsing
open Variable
open Types.Base
open Types.Tvar
open Env

type cf = CfWhile | CfCond
[@@deriving show]

type e =
| Abstract of typ
| Const of Ast.const
| Var of Variable.t
| Atom of atom
| Tag of tag * t
| Lambda of typ * Variable.t * t
| LambdaRec of (typ * Variable.t * t) list
| Ite of t * typ * t * t
| App of t * t
| Tuple of t list
| Cons of t * t
| Projection of Ast.projection * t
| RecordUpdate of t * string * t option
| Let of (typ list) * Variable.t * t * t
| TypeConstr of t * typ
| TypeCoerce of t * typ
| ControlFlow of cf * t * typ * t * t
[@@deriving show]
and t = Ast.exprid * e
[@@deriving show]

let map f =
  let rec aux (id,e) =
    let e =
      match e with
      | Abstract t -> Abstract t
      | Const c -> Const c
      | Var v -> Var v
      | Atom a -> Atom a
      | Tag (t,e) -> Tag (t, aux e)
      | Lambda (d, v, e) -> Lambda (d, v, aux e)
      | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (ty,v,aux e)) lst)
      | Ite (e, t, e1, e2) -> Ite (aux e, t, aux e1, aux e2)
      | App (e1, e2) -> App (aux e1, aux e2)
      | Tuple es -> Tuple (List.map aux es)
      | Cons (e1, e2) -> Cons (aux e1, aux e2)
      | Projection (p, e) -> Projection (p, aux e)
      | RecordUpdate (e, str, eo) -> RecordUpdate (aux e, str, Option.map aux eo)
      | Let (ta, v, e1, e2) -> Let (ta, v, aux e1, aux e2)
      | TypeConstr (e, ty) -> TypeConstr (aux e, ty)
      | TypeCoerce (e, ty) -> TypeCoerce (aux e, ty)
      | ControlFlow (cf, e, t, e1, e2) -> ControlFlow (cf, aux e, t, aux e1, aux e2)
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
    | Ite (e,_,e1,e2) | ControlFlow (_, e, _, e1, e2) -> [e ; e1 ; e2]
    | LambdaRec lst -> lst |> List.map (fun (_,_,e) -> e)
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
  | Abstract _ | Const _ | Atom _ | Tag _ | Ite _ | ControlFlow _
  | App _ | Tuple _ | Cons _ | Projection _
  | RecordUpdate _ | TypeConstr _ | TypeCoerce _ -> acc
  | Var v -> VarSet.add v acc
  | Let (_, v, _, _) | Lambda (_, v, _) -> VarSet.remove v acc
  | LambdaRec lst ->
    VarSet.diff acc (lst |> List.map (fun (_,v,_) -> v) |> VarSet.of_list)

let fv t = fold fv' t

let substitute' v v' (id,e) =
  let e = match e with
  | Var v'' when Variable.equals v v'' -> Var v'
  | e -> e
  in
  (id,e)
let substitute v v' e = map (substitute' v v') e

let apply_subst s e =
  let aux (id,e) =
    let e = match e with
    (* Ite and TypeConstr should not contain type variables *)
    | Abstract t -> Abstract (Subst.apply s t)
    | Lambda (ty,v,e) -> Lambda (Subst.apply s ty,v,e)
    | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (Subst.apply s ty, v, e)) lst)
    | Let (ts, v, e1, e2) -> Let (List.map (Subst.apply s) ts, v, e1, e2)
    | TypeCoerce (e, ty) -> TypeCoerce (e, Subst.apply s ty)
    | e -> e
    in id,e
  in
  map aux e

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
  let ts = pats |> List.map fst |> List.map type_of_pat in
  let t = disj ts in
  let body_of_pat pat e' =
    let vars = vars_of_pat pat in
    let add_def acc v =
      let d = def_of_var_pat pat v ex in
      (Ast.unique_exprid (), Ast.Let (v, d, acc))
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
  let body = (Ast.unique_exprid (), Ast.Suggest (x, ts, body)) in
  (id, Ast.Let (x, def, body))

let from_parser_ast t =
  let parts = PartitionTbl.create () in
  let let_binding x e1 e2 =
    Let (PartitionTbl.get_parts parts x, x, e1, e2)
  in
  let add_let x e =
    let x' = Variable.create_let (Variable.get_name x) in
    Variable.get_locations x |> List.iter (Variable.attach_location x') ;
    PartitionTbl.add_parts parts x' (PartitionTbl.get_parts parts x) ;
    Ast.unique_exprid (),
    let_binding x' (Ast.unique_exprid (), Var x) (substitute x x' e)
  in
  let lambda_annot x a =
    match a with
    | None -> TVar.mk ~user:false (Variable.get_name x) |> TVar.typ
    | Some d -> d
  in
  let rec aux_e (id,e) =
    match e with
    | Ast.Abstract t -> Abstract t
    | Ast.Const c -> Const c
    | Ast.Var v -> Var v
    | Ast.Atom a -> Atom a
    | Ast.Tag (t, e) -> Tag (t, aux e)
    | Ast.Suggest (v, tys, e) ->
      PartitionTbl.add_parts parts v tys ; aux_e e
    | Ast.Lambda (x, a, e) ->
      let e = aux e |> add_let x in
      Lambda (lambda_annot x a, x, e)
    | Ast.LambdaRec lst ->
      let aux (x,a,e) = (lambda_annot x a, x, aux e) in
      LambdaRec (List.map aux lst)
    | Ast.Ite (e,t,e1,e2) -> Ite (aux e, t, aux e1, aux e2)
    | Ast.App (e1,e2) -> App (aux e1, aux e2)
    | Ast.Let (x, e1, e2) -> let_binding x (aux e1) (aux e2)
    | Ast.Tuple es -> Tuple (List.map aux es)
    | Ast.Cons (e1, e2) -> Cons (aux e1, aux e2)
    | Ast.Projection (p, e) -> Projection (p, aux e)
    | Ast.RecordUpdate (e, lbl, eo) -> RecordUpdate (aux e, lbl, Option.map aux eo)
    | Ast.TypeConstr (e, ty) -> TypeConstr (aux e, ty)
    | Ast.TypeCoerce (e, ty) -> TypeCoerce (aux e, ty)
    | Ast.PatMatch (e, pats) -> encode_pattern_matching id e pats |> aux_e
    | Ast.Cond (e,t,e1,None) ->
      ControlFlow (CfCond, aux e, t, aux e1, (Ast.unique_exprid (), Const Unit))
    | Ast.Cond (e,t,e1,Some e2) -> ControlFlow (CfCond, aux e, t, aux e1, aux e2)
    | Ast.While (e,t,e1) ->
      ControlFlow (CfWhile, aux e, t, aux e1, (Ast.unique_exprid (), Const Unit))
  and aux t =
    let e = aux_e t in
    let (id, _) = t in
    (id,e)
  in
  aux t

let rec coerce ty (id,t) =
  try match t with
  | Let (tys, v, e1, e2) ->
    id, Let (tys, v, e1, coerce ty e2)
  | Lambda (da,v,e) ->
    let d = domain ty in
    let cd = apply ty d in
    if equiv ty (mk_arrow d cd) |> not then raise Exit ;
    begin match tallying (vars ty) [(d,da) ; (da,d)] with
    | [] -> raise Exit
    | s::_ ->
      let e = apply_subst s e in
      id, Lambda (d, v, coerce cd e)
    end
  | _ -> raise Exit
  with Exit -> Ast.unique_exprid (), TypeCoerce ((id,t), ty)
