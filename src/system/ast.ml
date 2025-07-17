open Variable
open Types.Base
open Types.Tvar

module Zd = struct
  type t = Z.t
  let pp = Z.pp_print
end
type const =
| Unit | Nil
| EmptyRecord
| Bool of bool
| Int of Zd.t
| Float of float
| Char of char
| String of string
[@@deriving show]

let typeof_const c =
  match c with
  | Unit -> unit_typ
  | Nil -> nil_typ
  | EmptyRecord -> empty_closed_record
  | Bool true -> true_typ
  | Bool false -> false_typ
  | Int i -> interval (Some i) (Some i)
  | Float _ -> float_typ
  | Char c -> char_interval c c
  | String str -> single_string str

(* -------------------- *)

type cf = CfWhile | CfCond
[@@deriving show]
type projection = Pi of int * int | Field of string | Hd | Tl | PiTag of tag
[@@deriving show]
type constructor = Tuple of int | Cons | RecUpd of string | RecDel of string | Tag of tag | Atom of atom
[@@deriving show]
type e =
| Abstract of typ
| Const of const
| Var of Variable.t
| Constructor of constructor * t list
| Atom of atom
| Tag of tag * t
| Lambda of typ * Variable.t * t
| LambdaRec of (typ * Variable.t * t) list
| Ite of t * typ * t * t
| App of t * t
| Tuple of t list
| Cons of t * t
| Projection of projection * t
| RecordUpdate of t * string * t option
| Let of (typ list) * Variable.t * t * t
| TypeConstr of t * typ
| TypeCoerce of t * typ
| ControlFlow of cf * t * typ * t * t
[@@deriving show]
and t = Eid.t * e
[@@deriving show]

let map f =
  let rec aux (id,e) =
    let e =
      match e with
      | Abstract t -> Abstract t
      | Const c -> Const c
      | Var v -> Var v
      | Constructor (c,es) -> Constructor (c, List.map aux es)
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
    | Constructor (_, es) | Tuple es -> es
    end
    |> List.map aux
    |> f (id,e)
  in
  aux

let fv' (_,e) accs =
  let acc = List.fold_left VarSet.union VarSet.empty accs in
  match e with
  | Abstract _ | Const _ | Constructor _ | Atom _ | Tag _ | Ite _
  | ControlFlow _ | App _ | Tuple _ | Cons _ | Projection _
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
  with Exit -> Eid.refresh id, TypeCoerce ((id,t), ty)
