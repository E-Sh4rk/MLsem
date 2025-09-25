open Mlsem_common
open Mlsem_types
module SA = Mlsem_system.Ast

type blockid = BFun | BLoop | BOther of int
[@@deriving show]
type pattern_constructor =
| PCTuple of int
| PCCons
| PCRec of string list * bool
| PCTag of Tag.t
| PCEnum of Enum.t
| PCCustom of SA.ccustom * SA.pcustom list
[@@deriving show]
type pattern =
| PType of Ty.t
| PVar of Variable.t
| PConstructor of pattern_constructor * pattern list
| PAnd of pattern * pattern
| POr of pattern * pattern
| PAssign of Variable.t * GTy.t
[@@deriving show]
type e =
| Hole of int
| Exc | Void | Voidify of t
| Isolate of t (* Prevent control flow encodings (CPS-like transformations) *)
| Value of GTy.t
| Var of Variable.t
| Constructor of SA.constructor * t list
| Lambda of Ty.t list (* Decomposition, similar to Let bindings *) * GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * Ty.t * t * t
| PatMatch of t * (pattern * t) list
| App of t * t
| Projection of SA.projection * t
| Declare of Variable.t * t (* Cannot be translated to system AST if v is not mutable *)
| Let of Ty.t list * Variable.t * t * t
| TypeCast of t * Ty.t
| TypeCoerce of t * GTy.t * SA.coerce
| VarAssign of Variable.t * t (* Cannot be translated to system AST if v is not mutable *)
| Try of t * t
| Seq of t * t
| Block of blockid * t
| Ret of blockid * t option
(* Imperative control flow *)
| If of t * Ty.t * t * t option
| While of t * Ty.t * t
| Return of t
| Break
[@@deriving show]
and t = Eid.t * e
[@@deriving show]

let map_pat_tl f pat =
  match pat with
  | PType ty -> PType ty
  | PVar v -> PVar v
  | PConstructor (p, ps) -> PConstructor (p, List.map f ps)
  | PAnd (p1, p2) -> PAnd (f p1, f p2)
  | POr (p1, p2) -> POr (f p1, f p2)
  | PAssign (v, ty) -> PAssign (v, ty)

let map_pat f =
  let rec aux pat =
    map_pat_tl aux pat |> f
  in
  aux

let map_pat' f =
  let rec aux pat =
    match f pat with
    | None -> map_pat_tl aux pat
    | Some pat -> pat
  in
  aux

let iter_pat f pat =
  let aux pat = f pat ; pat in
  map_pat aux pat |> ignore

let iter_pat' f pat =
  let aux pat = if f pat then None else Some pat in
  map_pat' aux pat |> ignore

let bv_pat pat =
  let bv = ref VarSet.empty in
  let aux = function
  | PVar v | PAssign (v, _) -> bv := VarSet.add v !bv
  | _ -> ()
  in
  iter_pat aux pat ; !bv

let map_tl f (id,e) =
  let e =
    match e with
    | Hole i -> Hole i
    | Exc -> Exc
    | Void -> Void
    | Voidify e -> Voidify (f e)
    | Isolate e -> Isolate (f e)
    | Value t -> Value t
    | Var v -> Var v
    | Constructor (c,es) -> Constructor (c, List.map f es)
    | Lambda (tys, d, v, e) -> Lambda (tys, d, v, f e)
    | LambdaRec lst ->
      LambdaRec (List.map (fun (ty,v,e) -> (ty,v,f e)) lst)
    | Ite (e, t, e1, e2) -> Ite (f e, t, f e1, f e2)
    | PatMatch (e, pats) ->
      PatMatch (f e, List.map (fun (pat, e) -> pat, f e) pats)
    | App (e1, e2) -> App (f e1, f e2)
    | Projection (p, e) -> Projection (p, f e)
    | Declare (v, e) -> Declare (v, f e)
    | Let (tys, v, e1, e2) -> Let (tys, v, f e1, f e2)
    | TypeCast (e, ty) -> TypeCast (f e, ty)
    | TypeCoerce (e, ty, b) -> TypeCoerce (f e, ty, b)
    | VarAssign (v, e) -> VarAssign (v, f e)
    | Try (e1, e2) -> Try (f e1, f e2)
    | Seq (e1, e2) -> Seq (f e1, f e2)
    | Block (bid, e) -> Block (bid, f e)
    | Ret (bid, eo) -> Ret (bid, Option.map f eo)
    | If (e, ty, e1, e2) -> If (f e, ty, f e1, Option.map f e2)
    | While (e, ty, e') -> While (f e, ty, f e')
    | Return e -> Return (f e)
    | Break -> Break
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
  | PatMatch (_,pats) ->
    bv := List.fold_left (fun acc (pat,_) -> VarSet.union acc (bv_pat pat)) !bv pats
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
