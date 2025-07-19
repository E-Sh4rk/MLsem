open Types.Base
open Types.Tvar
open Types.Gradual
open System.Ast
open Variable

let rec type_of_pat pat =
  let open Ast in
  match pat with
  | PatType t -> t
  | PatLit c -> typeof_const c
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
  | PatType _ | PatLit _ -> VarSet.empty
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
    def_of_var_pat p v (Eid.unique (), Projection (PiTag tag, e))
  | PatAnd (p1, p2) ->
    if vars_of_pat p1 |> VarSet.mem v
    then def_of_var_pat p1 v e
    else def_of_var_pat p2 v e
  | PatTuple ps ->
    let i = List.find_index (fun p -> vars_of_pat p |> VarSet.mem v) ps |> Option.get in
    let n = List.length ps in
    let p = List.nth ps i in
    def_of_var_pat p v (Eid.unique (), Projection (Pi (n,i), e))
  | PatCons (p1, p2) ->
    if vars_of_pat p1 |> VarSet.mem v
    then def_of_var_pat p1 v (Eid.unique (), Projection (Hd, e))
    else def_of_var_pat p2 v (Eid.unique (), Projection (Tl, e))
  | PatRecord (fields, _) ->
    let (str, p) =
      fields |> List.find (fun (_, p) -> vars_of_pat p |> VarSet.mem v)
    in
    def_of_var_pat p v (Eid.unique (), Projection (Field str, e))
  | PatOr (p1, p2) ->
    let case = Ite (e, type_of_pat p1,
      def_of_var_pat p1 v e, def_of_var_pat p2 v e) in
    (Eid.unique (), case)
  | PatAssign (v', c) when Variable.equals v v' -> (Eid.unique (), Const c)
  | PatAssign _ -> assert false
  | PatType _ -> assert false
  | PatLit _ -> assert false

let encode_pattern_matching e pats =
  let x = Variable.create_gen None in
  let ex : Ast.expr = (Eid.unique (), Var x) in
  let ts = pats |> List.map fst |> List.map type_of_pat in
  let t = disj ts in
  let body_of_pat pat e' =
    let vars = vars_of_pat pat in
    let add_def acc v =
      let d = def_of_var_pat pat v ex in
      (Eid.unique (), Ast.Let (v, d, acc))
    in
    List.fold_left add_def e' (VarSet.elements vars)
  in
  let add_branch acc (t, e') =
    (Eid.unique (), Ast.Ite (ex, t, e', acc))
  in
  let pats = pats |> List.map (fun (pat, e') ->
    (type_of_pat pat, body_of_pat pat e')) |> List.rev in
  let body = match pats with
  | [] -> assert false
  | (_, e')::pats -> List.fold_left add_branch e' pats
  in
  let def = (Eid.refresh (fst e), Ast.TypeCast (e, t)) in
  let body = (Eid.unique (), Ast.Suggest (x, ts, body)) in
  Ast.Let (x, def, body)

let expr_to_ast t =
  let sugg : (Variable.t, typ list) Hashtbl.t = Hashtbl.create 100 in
  let get_sugg v =
    match Hashtbl.find_opt sugg v with Some lst -> lst | None -> []
  in
  let add_suggs v tys =
    Hashtbl.replace sugg v (tys@(get_sugg v))
  in
  let let_binding x e1 e2 =
    Let (get_sugg x, x, e1, e2)
  in
  let add_let x e =
    let x' = Variable.create_let (Variable.get_name x) in
    Variable.get_location x |> Variable.attach_location x' ;
    add_suggs x' (get_sugg x) ;
    Eid.refresh (fst e),
    let_binding x' (Eid.unique (), Var x) (substitute x x' e)
  in
  let lambda_annot x a =
    match a with
    | None -> TVar.mk ~user:false (Variable.get_name x) |> TVar.typ |> GTy.mk
    | Some d -> GTy.mk d
  in
  let rec aux_e e =
    match e with
    | Ast.Abstract t -> Abstract (GTy.mk t)
    | Ast.Const c -> Const c
    | Ast.Var v -> Var v
    | Ast.Atom a -> Constructor (Atom a, [])
    | Ast.Tag (t, e) -> Constructor (Tag t, [aux e])
    | Ast.Suggest (v, tys, (_,e)) ->
      add_suggs v tys ; aux_e e
    | Ast.Lambda (x, a, e) ->
      let e = aux e |> add_let x in
      Lambda (lambda_annot x a, x, e)
    | Ast.LambdaRec lst ->
      let aux (x,a,e) = (lambda_annot x a, x, aux e) in
      LambdaRec (List.map aux lst)
    | Ast.Ite (e,t,e1,e2) -> Ite (aux e, t, aux e1, aux e2)
    | Ast.App (e1,e2) -> App (aux e1, aux e2)
    | Ast.Let (x, e1, e2) -> let_binding x (aux e1) (aux e2)
    | Ast.Tuple es -> Constructor (Tuple (List.length es), List.map aux es)
    | Ast.Cons (e1, e2) -> Constructor (Cons, [aux e1 ; aux e2])
    | Ast.Projection (p, e) -> Projection (p, aux e)
    | Ast.RecordUpdate (e, lbl, None) -> Constructor (RecDel lbl, [aux e])
    | Ast.RecordUpdate (e, lbl, Some e') -> Constructor (RecUpd lbl, [aux e ; aux e'])
    | Ast.TypeCast (e, ty) -> TypeCast (aux e, ty)
    | Ast.TypeCoerce (e, ty, b) -> TypeCoerce (aux e, GTy.mk ty, b)
    | Ast.PatMatch (e, pats) -> encode_pattern_matching e pats |> aux_e
    | Ast.Cond (e,t,e1,None) ->
      ControlFlow (CfCond, aux e, t, aux e1, (Eid.unique (), Const Unit))
    | Ast.Cond (e,t,e1,Some e2) -> ControlFlow (CfCond, aux e, t, aux e1, aux e2)
    | Ast.While (e,t,e1) ->
      ControlFlow (CfWhile, aux e, t, aux e1, (Eid.unique (), Const Unit))
    | Ast.Seq (e1, e2) -> Let ([], Variable.create_gen None, aux e1, aux e2)
  and aux (id, e) =
    (id, aux_e e)
  in
  aux t
