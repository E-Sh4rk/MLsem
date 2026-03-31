open Mlsem_common
open Mlsem_types

type pcustom = { pname: string ; pdom: Ty.t -> Ty.t ; proj: Ty.t -> Ty.t ; pgen: bool }
let pp_pcustom fmt pc = Format.fprintf fmt "%s" pc.pname
type ccustom = { cname: string ; cdom: Ty.t -> Ty.t list list ; cons: Ty.t list -> Ty.t ; cgen: bool }
let pp_ccustom fmt cc = Format.fprintf fmt "%s" cc.cname
type ocustom = { oname: string ; ofun: TyScheme.t ; ogen: bool }
let pp_ocustom fmt oc = Format.fprintf fmt "%s" oc.oname
type check = Check | CheckStatic | NoCheck
[@@deriving show]
type projection =
| Pi of int * int | PiField of string | PiFieldOpt of string
| Hd | Tl | PiTag of Tag.t | PCustom of pcustom
[@@deriving show]
type constructor =
| Tuple of int | Cons | Rec of string list * bool | Tag of Tag.t | Enum of Enum.t 
| Join of int | Meet of int | Negate | Ternary of Ty.t (* Should not contain type vars *)
| Normalize | CCustom of ccustom
[@@deriving show]
type operation =
| RecUpd of string | RecDel of string
| OCustom of ocustom
[@@deriving show]
type e =
| Value of GTy.t
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * GTy.t * t * t
| App of t * t
| Operation of operation * t
| Projection of projection * t
| Let of (Ty.t list) * Variable.t * t * t
| TypeCast of t * GTy.t * check
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
    | Constructor (c, es) -> Constructor (c, List.map f es)
    | Lambda (d, v, e) -> Lambda (d, v, f e)
    | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (ty,v,f e)) lst)
    | Ite (e, t, e1, e2) -> Ite (f e, t, f e1, f e2)
    | App (e1, e2) -> App (f e1, f e2)
    | Operation (o, e) -> Operation (o, f e)
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
    | Value t -> Value (GTy.substitute s t)
    | Lambda (ty,v,e) -> Lambda (GTy.substitute s ty,v,e)
    | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (GTy.substitute s ty, v, e)) lst)
    | Let (ts, v, e1, e2) -> Let (List.map (Subst.apply s) ts, v, e1, e2)
    | Ite (e, ty, e1, e2) -> Ite (e, GTy.substitute s ty, e1, e2)
    | TypeCoerce (e, ty, b) -> TypeCoerce (e, GTy.substitute s ty, b)
    | TypeCast (e, ty, b) -> TypeCast (e, GTy.substitute s ty, b)
    | e -> e
    in id,e
  in
  map aux e

let coerce c ty (id,t) =
  let mono = GTy.fv ty |> MVarSet.filter (TVar.has_kind KNoInfer) (RVar.has_kind KNoInfer) in
  let cs = ref Subst.identity in
  let rec aux ty (id,t) =
    let unify ty1 ty2 =
      let s = !cs in
      let ty1, ty2 = GTy.substitute s ty1, GTy.substitute s ty2 in
      match TVOp.tallying mono
        [(GTy.lb ty1, GTy.lb ty2) ; (GTy.lb ty2, GTy.lb ty1) ;
        (GTy.ub ty1, GTy.ub ty2) ; (GTy.ub ty2, GTy.ub ty1)]
      with
      | [] -> raise Exit
      | s'::_ -> cs := Subst.compose s' s
    in
    try match t with
    | Let (tys, v, e1, e2) ->
      id, Let (tys, v, e1, aux ty e2)
    | Ite (e, tau, e1, e2) ->
      id, Ite (e, tau, aux ty e1, aux ty e2)
    | Lambda (da,v,e) ->
      let d = GTy.map Arrow.domain ty in
      let cd = GTy.map2 Arrow.apply ty d in
      if GTy.equiv ty (GTy.map2 Arrow.mk d cd) |> not then raise Exit ;
      unify d da ; id, Lambda (d, v, aux cd e)
    | LambdaRec lst ->
      let n = List.length lst in
      let tys = List.mapi (fun i _ -> GTy.map (Tuple.proj n i) ty) lst in
      if GTy.equiv ty (GTy.mapl Tuple.mk tys) |> not then raise Exit ;
      id, LambdaRec (List.combine lst tys |>
          List.map (fun ((tya,v,e), ty) -> unify ty tya ; (ty,v,aux ty e))
        )
    | _ -> raise Exit
    with Exit -> Eid.refresh id, TypeCoerce ((id,t), ty, c)
  in
  let res = aux ty (id,t) in
  apply_subst !cs res
