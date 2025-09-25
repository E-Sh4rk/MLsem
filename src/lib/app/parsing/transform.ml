open Mlsem_common
open Mlsem_types
module MVariable = Mlsem_lang.MVariable

let expr_to_ast t =
  let open Mlsem_lang.Ast in
  let sugg = Hashtbl.create 100 in
  let get_sugg v =
    match Hashtbl.find_opt sugg v with Some lst -> lst | None -> []
  in
  let add_suggs v tys =
    Hashtbl.replace sugg v (tys@(get_sugg v))
  in
  let lambda_annot name a =
    match a with
    | None -> TVar.mk KInfer name |> TVar.typ |> GTy.mk
    | Some d -> GTy.mk d
  in
  let rec aux_pat = function
    | PAst.PatType ty -> PType ty
    | PatVar (_, v) -> PVar v
    | PatLit c -> PType (Mlsem_lang.Const.typeof c)
    | PatTag (t, pat) -> PConstructor (PCTag t, [aux_pat pat])
    | PatTuple pats -> PConstructor (PCTuple (List.length pats), List.map aux_pat pats)
    | PatCons (p1, p2) -> PConstructor (PCCons, [aux_pat p1; aux_pat p2])
    | PatRecord (fields, opened) ->
      PConstructor (PCRec (List.map fst fields, opened), fields |> List.map snd |> List.map aux_pat)
    | PatAnd (p1, p2) -> PAnd (aux_pat p1, aux_pat p2)
    | PatOr (p1, p2) -> POr (aux_pat p1, aux_pat p2)
    | PatAssign ((_,v), c) -> PAssign (v, Mlsem_lang.Const.typeof c |> GTy.mk)
  in
  let rec aux_e e =
    match e with
    | PAst.Magic t -> Value (GTy.mk t)
    | Const c -> Value (Mlsem_lang.Const.typeof c |> GTy.mk)
    | Var v -> Var v
    | Enum e -> Constructor (Enum e, [])
    | Tag (t, e) -> Constructor (Tag t, [aux e])
    | Suggest (v, tys, (_,e)) ->
      add_suggs v tys ; aux_e e
    | Lambda (x, a, e) ->
      let x' = MVariable.refresh MVariable.Immut x in
      let e = aux e |> rename_fv x x' in
      Lambda (get_sugg x, lambda_annot (Variable.get_name x) a, x', e)
    | LambdaRec lst ->
      let lst = lst |> List.map (fun (x,a,e) ->
        x, MVariable.refresh MVariable.Immut x, a, e) in
      let aux (x,x',a,e) =
        lambda_annot (Variable.get_name x) a, x',
        List.fold_left (fun e (x,x',_,_) -> rename_fv x x' e) (aux e) lst
      in
      LambdaRec (List.map aux lst)
    | Ite (e,t,e1,e2) -> Ite (aux e, t, aux e1, aux e2)
    | App (e1,e2) -> App (aux e1, aux e2)
    | Let ((_,x), e1, e2) ->
      let e1, e2 = aux e1, aux e2 in
      Let (get_sugg x, x, e1, e2)
    | Tuple es -> Constructor (Tuple (List.length es), List.map aux es)
    | Cons (e1, e2) -> Constructor (Cons, [aux e1 ; aux e2])
    | Projection (p, e) -> Projection (p, aux e)
    | Record lst ->
      Constructor (Rec (List.map (fun (str,_) -> str, false) lst, false), List.map snd lst |> List.map aux)
    | RecordUpdate (e, lbl, None) -> Constructor (RecDel lbl, [aux e])
    | RecordUpdate (e, lbl, Some e') -> Constructor (RecUpd lbl, [aux e ; aux e'])
    | TypeCast (e, ty) -> TypeCast (aux e, ty)
    | TypeCoerce (e, tyo, c) ->
      let ty = match tyo with None -> GTy.dyn | Some ty -> GTy.mk ty in
      TypeCoerce (aux e, ty, c)
    | VarAssign (v, e) -> VarAssign (v, aux e)
    | PatMatch (e, pats) -> PatMatch (aux e, List.map (fun (pat,e) -> (aux_pat pat,aux e)) pats)
    | Cond (e,t,e1,e2) -> If (aux e, t, aux e1, Option.map aux e2)
    | While (e,t,e1) -> While (aux e, t, aux e1)
    | Seq (e1, e2) -> Seq (aux e1, aux e2)
    | Return e -> Return (aux e)
    | Break | Continue -> Break
  and aux (id, e) =
    (id, aux_e e)
  in
  aux t |> Mlsem_lang.Transform.transform
