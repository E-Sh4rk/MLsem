open Mlsem_common
open Mlsem_types
module SA = Mlsem_system.Ast

type e =
| Hole of int
| Exc | Void | Voidify of t
| Value of GTy.t
| Var of Variable.t
| Constructor of SA.constructor * t list
| Lambda of Ty.t list (* Decomposition, similar to Let bindings *) * GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * Ty.t * t * t
| App of t * t
| Projection of SA.projection * t
| Declare of Variable.t * t (* Cannot be translated to system AST if v is not mutable *)
| Let of Ty.t list * Variable.t * t * t
| TypeCast of t * Ty.t
| TypeCoerce of t * GTy.t * SA.coerce
| VarAssign of Variable.t * t (* Cannot be translated to system AST if v is not mutable *)
| Seq of t * t
| Try of t * t
[@@deriving show]
and t = Eid.t * e
[@@deriving show]

let map_tl f (id,e) =
  let e =
    match e with
    | Hole n -> Hole n
    | Exc -> Exc | Void -> Void
    | Voidify e -> Voidify (f e)
    | Value t -> Value t
    | Var v -> Var v
    | Constructor (c,es) -> Constructor (c, List.map f es)
    | Lambda (tys, d, v, e) -> Lambda (tys, d, v, f e)
    | LambdaRec lst ->
      LambdaRec (List.map (fun (ty,v,e) -> (ty,v,f e)) lst)
    | Ite (e, t, e1, e2) -> Ite (f e, t, f e1, f e2)
    | App (e1, e2) -> App (f e1, f e2)
    | Projection (p, e) -> Projection (p, f e)
    | Declare (v, e) -> Declare (v, f e)
    | Let (tys, v, e1, e2) -> Let (tys, v, f e1, f e2)
    | TypeCast (e, ty) -> TypeCast (f e, ty)
    | TypeCoerce (e, ty, b) -> TypeCoerce (f e, ty, b)
    | VarAssign (v, e) -> VarAssign (v, f e)
    | Seq (e1, e2) -> Seq (f e1, f e2)
    | Try (e1, e2) -> Try (f e1, f e2)
  in
  (id,e)

let map f =
  let rec aux e =
    map_tl aux e |> f
  in
  aux

let map' f =
  let rec aux e =
    match f e with
    | None -> map_tl aux e
    | Some e -> e
  in
  aux

let iter f e =
  let aux e = f e ; e in
  map aux e |> ignore

let iter' f e =
  let aux e = if f e then None else Some e in
  map' aux e |> ignore

let fill_hole n elt e =
  map (function (_, Hole i) when i=n -> elt | e -> e) e

let bv e =
  let bv = ref VarSet.empty in
  let aux (_,e) = match e with
  | Lambda (_, _, v, _) | Let (_, v, _, _) | Declare (v, _) -> bv := VarSet.add v !bv
  | LambdaRec lst -> lst |> List.iter (fun (_, v, _) -> bv := VarSet.add v !bv)
  | _ -> ()
  in
  iter aux e ; !bv

let uv e =
  let uv = ref VarSet.empty in
  let aux (_,e) = match e with
  | Var v | VarAssign (v,_) -> uv := VarSet.add v !uv
  | _ -> ()
  in
  iter aux e ; !uv

let fv e = VarSet.diff (uv e) (bv e)
let vars e = VarSet.union (uv e) (bv e)

let rename_fv v v' =
  let aux (id, e) =
    let e = match e with
    | Var v'' when Variable.equals v v'' -> Var v'
    | VarAssign (v'', e) when Variable.equals v v'' -> VarAssign (v', e)
    | e -> e
    in
    (id, e)
  in
  map aux

(* === Encoding to System.Ast === *)

let to_system_ast t =
  let rec aux (id, e) =
    let e = match e with
    | Exc -> SA.Value (GTy.mk Ty.empty)
    | Void -> SA.Value (GTy.mk !Config.void_ty)
    | Voidify e -> SA.Constructor (SA.Ignore !Config.void_ty, [aux e])
    | Value t -> SA.Value t
    | Var v ->
      if MVariable.is_mutable v then
        SA.App ((Eid.unique (), SA.Value (MVariable.ref_get v |> GTy.mk)),
                (Eid.unique (), SA.Var v))
      else
        SA.Var v
    | Constructor (c, es) -> SA.Constructor (c, List.map aux es)
    | Lambda (tys, ty, x, e) ->
      if MVariable.is_mutable x then invalid_arg "Variable of Lambda cannot be mutable." ;
      let x' = MVariable.create_let Immut (Variable.get_name x) in
      Variable.get_location x |> Variable.attach_location x' ;
      let body =
        Eid.refresh (fst e),
        Let (tys, x', (Eid.unique (), Var x), rename_fv x x' e)
      in
      SA.Lambda (ty, x, aux body)
    | LambdaRec lst ->
      let aux (ty,x,e) = (ty, x, aux e) in
      if lst |> List.exists (fun (_,v,_) -> MVariable.is_mutable v)
      then invalid_arg "Variables of LambdaRec cannot be mutable." ;
      SA.LambdaRec (List.map aux lst)
    | Ite (e,t,e1,e2) -> SA.Ite (aux e, t, aux e1, aux e2)
    | App (e1,e2) -> SA.App (aux e1, aux e2)
    | Projection (p, e) -> SA.Projection (p, aux e)
    | Declare (x, e) when MVariable.is_mutable x ->
      let def = Eid.unique (), SA.App (
          (Eid.unique (), SA.Value (MVariable.ref_uninit x |> GTy.mk)),
          (Eid.unique (), SA.Value (Ty.unit |> GTy.mk))) in
      SA.Let ([], x, def, aux e)
    | Declare _ -> invalid_arg "Cannot declare an immutable variable."
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
    | Seq (e1, e2) -> Let ([], Variable.create_gen None, aux e1, aux e2)
    | Try (e1, e2) -> SA.Constructor (SA.Choice 2, [aux e1 ; aux e2])
    | Hole _ -> invalid_arg "Expression should not contain a hole."
    in
    (id, e)
  in
  aux t
