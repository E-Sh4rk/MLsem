open Variable
open Types.Base
open Types.Tvar
open Types.Gradual

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
| Abstract of GTy.t
| Const of const
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * typ * t * t
| App of t * t
| Projection of projection * t
| Let of (typ list) * Variable.t * t * t
| TypeCast of t * typ
| TypeCoerce of t * GTy.t
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
      | Lambda (d, v, e) -> Lambda (d, v, aux e)
      | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (ty,v,aux e)) lst)
      | Ite (e, t, e1, e2) -> Ite (aux e, t, aux e1, aux e2)
      | App (e1, e2) -> App (aux e1, aux e2)
      | Projection (p, e) -> Projection (p, aux e)
      | Let (ta, v, e1, e2) -> Let (ta, v, aux e1, aux e2)
      | TypeCast (e, ty) -> TypeCast (aux e, ty)
      | TypeCoerce (e, ty) -> TypeCoerce (aux e, ty)
      | ControlFlow (cf, e, t, e1, e2) -> ControlFlow (cf, aux e, t, aux e1, aux e2)
    in
    f (id,e)
  in
  aux

let fold f =
  let rec aux (id,e) =
    begin match e with
    | Abstract _ | Const _ | Var _ -> []
    | Lambda (_,_, e) | Projection (_, e) | TypeCast (e,_) | TypeCoerce (e,_) -> [e]
    | Ite (e,_,e1,e2) | ControlFlow (_, e, _, e1, e2) -> [e ; e1 ; e2]
    | LambdaRec lst -> lst |> List.map (fun (_,_,e) -> e)
    | App (e1,e2) | Let (_,_,e1,e2) -> [e1 ; e2]
    | Constructor (_, es) -> es
    end
    |> List.map aux
    |> f (id,e)
  in
  aux

let fv' (_,e) accs =
  let acc = List.fold_left VarSet.union VarSet.empty accs in
  match e with
  | Abstract _ | Const _ | Constructor _ | Ite _ | ControlFlow _
  | App _ | Projection _  | TypeCast _ | TypeCoerce _ -> acc
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
    (* Ite and TypeCast should not contain type variables *)
    | Abstract t -> Abstract (GTy.substitute s t)
    | Lambda (ty,v,e) -> Lambda (GTy.substitute s ty,v,e)
    | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (GTy.substitute s ty, v, e)) lst)
    | Let (ts, v, e1, e2) -> Let (List.map (Subst.apply s) ts, v, e1, e2)
    | TypeCoerce (e, ty) -> TypeCoerce (e, GTy.substitute s ty)
    | e -> e
    in id,e
  in
  map aux e

let rec coerce ty (id,t) =
  let unify ty1 ty2 =
    match tallying (GTy.fv ty)
      [(GTy.lb ty1, GTy.lb ty2) ; (GTy.lb ty2, GTy.lb ty1) ;
       (GTy.ub ty1, GTy.ub ty2) ; (GTy.ub ty2, GTy.ub ty1)]
    with
    | [] -> raise Exit
    | s::_ -> s
  in
  try match t with
  | Let (tys, v, e1, e2) ->
    id, Let (tys, v, e1, coerce ty e2)
  | Lambda (da,v,e) ->
    let d = GTy.map domain ty in
    let cd = GTy.map2 apply ty d in
    if GTy.equiv ty (GTy.map2 mk_arrow d cd) |> not then raise Exit ;
    let s = unify d da in
    let e = apply_subst s e |> coerce cd in
    id, Lambda (d, v, e)
  | LambdaRec lst ->
    let n = List.length lst in
    let tys = List.mapi (fun i _ -> GTy.map (pi n i) ty) lst in
    if GTy.equiv ty (GTy.mapl mk_tuple tys) |> not then raise Exit ;
    id, LambdaRec (List.combine lst tys |> List.map (fun ((tya,v,e), ty) ->
      let s = unify ty tya in
      let e = apply_subst s e |> coerce ty in
      (ty,v,e))
      )
  | _ -> raise Exit
  with Exit -> Eid.refresh id, TypeCoerce ((id,t), ty)
