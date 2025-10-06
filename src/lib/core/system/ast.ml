open Mlsem_common
open Mlsem_types

type pcustom = { pname: string ; pdom: Ty.t -> Ty.t ; proj: Ty.t -> Ty.t ; pgen: bool }
let pp_pcustom fmt pc = Format.fprintf fmt "%s" pc.pname
type ccustom = { cname: string ; cdom: Ty.t -> Ty.t list list ; cons: Ty.t list -> Ty.t ; cgen: bool }
let pp_ccustom fmt cc = Format.fprintf fmt "%s" cc.cname
type check = Check | CheckStatic | NoCheck
[@@deriving show]
type projection =
| Pi of int * int | Field of string | Hd | Tl | PiTag of Tag.t
| PCustom of pcustom
[@@deriving show]
type constructor =
| Tuple of int | Cons | Rec of (string * bool) list * bool | Tag of Tag.t | Enum of Enum.t 
| RecUpd of string | RecDel of string | Choice of int | Ignore of Ty.t (* Should not contain type vars *)
| CCustom of ccustom
[@@deriving show]
type e =
| Value of GTy.t
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * Ty.t * t * t
| App of t * t
| Projection of projection * t
| Let of (Ty.t list) * Variable.t * t * t
| TypeCast of t * Ty.t * check
| TypeCoerce of t * GTy.t * check
| Alt of t * t
[@@deriving show]
and t = Eid.t * e
[@@deriving show]

let map_tl f (id,e) =
  let e =
    match e with
    | Value t -> Value t
    | Var v -> Var v
    | Constructor (c,es) -> Constructor (c, List.map f es)
    | Lambda (d, v, e) -> Lambda (d, v, f e)
    | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (ty,v,f e)) lst)
    | Ite (e, t, e1, e2) -> Ite (f e, t, f e1, f e2)
    | App (e1, e2) -> App (f e1, f e2)
    | Projection (p, e) -> Projection (p, f e)
    | Let (ta, v, e1, e2) -> Let (ta, v, f e1, f e2)
    | TypeCast (e, ty, c) -> TypeCast (f e, ty, c)
    | TypeCoerce (e, ty, c) -> TypeCoerce (f e, ty, c)
    | Alt (e1, e2) -> Alt (f e1, f e2)
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

let bv e =
  let bv = ref VarSet.empty in
  let aux (_,e) = match e with
  | Lambda (_, v, _) | Let (_, v, _, _) -> bv := VarSet.add v !bv
  | LambdaRec lst -> lst |> List.iter (fun (_, v, _) -> bv := VarSet.add v !bv)
  | _ -> ()
  in
  iter aux e ; !bv

let uv e =
  let uv = ref VarSet.empty in
  let aux (_,e) = match e with
  | Var v -> uv := VarSet.add v !uv
  | _ -> ()
  in
  iter aux e ; !uv

let fv e = VarSet.diff (uv e) (bv e)
let vars e = VarSet.union (uv e) (bv e)

let apply_subst s e =
  let aux (id,e) =
    let e = match e with
    (* Ite, Conditional, and TypeCast should not contain type variables *)
    | Value t -> Value (GTy.substitute s t)
    | Lambda (ty,v,e) -> Lambda (GTy.substitute s ty,v,e)
    | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (GTy.substitute s ty, v, e)) lst)
    | Let (ts, v, e1, e2) -> Let (List.map (Subst.apply s) ts, v, e1, e2)
    | TypeCoerce (e, ty, b) -> TypeCoerce (e, GTy.substitute s ty, b)
    | e -> e
    in id,e
  in
  map aux e

let rec coerce c ty (id,t) =
  let unify ty1 ty2 =
    match TVOp.tallying (GTy.fv ty)
      [(GTy.lb ty1, GTy.lb ty2) ; (GTy.lb ty2, GTy.lb ty1) ;
       (GTy.ub ty1, GTy.ub ty2) ; (GTy.ub ty2, GTy.ub ty1)]
    with
    | [] -> raise Exit
    | s::_ -> s
  in
  try match t with
  | Let (tys, v, e1, e2) ->
    id, Let (tys, v, e1, coerce c ty e2)
  | Ite (e, tau, e1, e2) ->
    id, Ite (e, tau, coerce c ty e1, coerce c ty e2)
  | Lambda (da,v,e) ->
    let d = GTy.map Arrow.domain ty in
    let cd = GTy.map2 Arrow.apply ty d in
    if GTy.equiv ty (GTy.map2 Arrow.mk d cd) |> not then raise Exit ;
    let s = unify d da in
    let e = apply_subst s e |> coerce c cd in
    id, Lambda (d, v, e)
  | LambdaRec lst ->
    let n = List.length lst in
    let tys = List.mapi (fun i _ -> GTy.map (Tuple.proj n i) ty) lst in
    if GTy.equiv ty (GTy.mapl Tuple.mk tys) |> not then raise Exit ;
    id, LambdaRec (List.combine lst tys |> List.map (fun ((tya,v,e), ty) ->
      let s = unify ty tya in
      let e = apply_subst s e |> coerce c ty in
      (ty,v,e))
      )
  | _ -> raise Exit
  with Exit -> Eid.refresh id, TypeCoerce ((id,t), ty, c)
