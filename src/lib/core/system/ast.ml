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
| Join of int | Meet of int | Ternary of Ty.t (* Should not contain type vars *)
| Voidify of Ty.t (* Should not contain type vars *)
| Normalize | CCustom of ccustom
[@@deriving show]
type operation =
| RecUpd of string | RecDel of string
| OCustom of ocustom
[@@deriving show]
type param_annot = GTy.t option
[@@deriving show]
type e =
| Value of TyScheme.t
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of param_annot * Variable.t * t
| LambdaRec of (param_annot * Variable.t * t) list
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
    | Value t -> Value (TyScheme.substitute s t)
    | Lambda (ty,v,e) -> Lambda (Option.map (GTy.substitute s) ty,v,e)
    | LambdaRec lst -> LambdaRec (List.map (fun (ty,v,e) -> (Option.map (GTy.substitute s) ty, v, e)) lst)
    | Let (ts, v, e1, e2) -> Let (List.map (Subst.apply s) ts, v, e1, e2)
    | Ite (e, ty, e1, e2) -> Ite (e, GTy.substitute s ty, e1, e2)
    | TypeCoerce (e, ty, b) -> TypeCoerce (e, GTy.substitute s ty, b)
    | TypeCast (e, ty, b) -> TypeCast (e, GTy.substitute s ty, b)
    | e -> e
    in id,e
  in
  map aux e

(* Projections and constructors *)

let domain_of_proj p ty =
  match p with
  | PiField label -> Record.mk_open [label, (ty, false)]
  | PiFieldOpt label -> Record.mk_open [label, (ty, true)]
  | Pi(n,i) ->
    if i >= n then Ty.empty
    else Tuple.mk (List.init n (fun k -> if k=i then ty else Ty.any))
  | Hd ->
    Lst.cons ty Lst.any
  | Tl ->
    Lst.cons Ty.any ty
  | PiTag tag ->
    Tag.mk tag ty
  | PCustom r -> r.pdom ty

let proj p ty =
  match p with
  | PiField label | PiFieldOpt label -> Record.proj ty label
  | Pi (n,i) -> Tuple.proj n i ty
  | Hd -> Lst.proj ty |> fst
  | Tl -> Lst.proj ty |> snd
  | PiTag tag -> Tag.proj tag ty
  | PCustom r -> r.proj ty

let domains_of_construct (c:constructor) ty =
  match c with
  | Tuple n ->
    Tuple.dnf n ty
    |> List.filter (fun b -> Ty.leq (Tuple.mk b) ty)
  | Join n | Meet n -> [List.init n (fun _ -> ty)]
  | Normalize when Ty.is_any ty -> [ [Ty.any] ]
  | Normalize -> [ ]
  | Voidify ty' when Ty.leq ty' ty -> [ [Ty.any] ]
  | Voidify _ -> [ ]
  | Ternary _ -> [ [ Ty.any ; ty ; ty ] ]
  | Cons ->
    Lst.dnf ty
    |> List.filter (fun (a,b) -> Ty.leq (Lst.cons a b) ty)
    |> List.map (fun (t1,t2) -> [t1;t2])
  | Rec (labels, opened) ->
    let mk = if opened then Record.mk_open else Record.mk_closed in
    Ty.cap ty (mk (List.map (fun lbl -> (lbl, (Ty.any,false))) labels))
    |> Record.dnf
    |> List.filter_map (fun (bindings,tail) ->
      let ty' = Record.mk tail bindings in
      if Ty.leq ty' ty
      then Some (List.map (fun lbl -> Record.proj ty' lbl) labels)
      else None
    )
  | Tag tag -> [ [ Tag.proj tag ty ] ]
  | Enum e when Ty.leq (Enum.typ e) ty -> [ [] ]
  | Enum _ -> [ ]
  | CCustom r -> r.cdom ty

let construct (c:constructor) tys =
  match c, tys with
  | Tuple n, tys when List.length tys = n -> Tuple.mk tys
  | Join n, tys when List.length tys = n -> Ty.disj tys
  | Meet n, tys when List.length tys = n -> Ty.conj tys
  | Normalize, [ty] -> !Config.normalization_fun ty
  | Voidify ty, [_] -> ty
  | Ternary tau, [t;t1;t2] ->
    if Ty.leq t tau then t1
    else if Ty.leq t (Ty.neg tau) then t2
    else Ty.cup t1 t2
  | Cons, [t1 ; t2] -> Lst.cons t1 t2
  | Rec (labels, opened), tys when List.length labels = List.length tys ->
    let mk = if opened then Record.mk_open else Record.mk_closed in
    let bindings = List.map2 (fun lbl ty -> (lbl, (ty,false))) labels tys in
    mk bindings
  | Tag tag, [t] -> Tag.mk tag t
  | Enum enum, [] -> Enum.typ enum
  | CCustom r, tys -> r.cons tys
  | _ -> raise (Invalid_argument "Invalid arity for constructor.")

let rv = RVar.mk KNoInfer None
let tv = TVar.mk KNoInfer None
let fun_of_operation o =
  match o with
  | RecUpd lbl ->
    let dom1, dom2 = Record.mk' (RVar.fty rv) [], TVar.typ tv in
    let codom = Record.mk' (RVar.fty rv) [(lbl, FTy.of_oty (TVar.typ tv, false))] in
    Arrow.mk (Tuple.mk [dom1;dom2]) codom |> GTy.mk |> TyScheme.mk_poly
  | RecDel lbl ->
    let dom = Record.mk' (RVar.fty rv) [] in
    let codom = Record.mk' (RVar.fty rv) [(lbl, FTy.of_oty (Ty.empty, true))] in
    Arrow.mk dom codom |> GTy.mk |> TyScheme.mk_poly
  | OCustom { ofun ; _ } -> ofun

let coerce ?coercion_id c ty t =
  let mono = TVOp.all_vars KNoInfer in
  let rec aux ?coercion_id ty (id,t) =
    let unify ty1 ty2 =
      match TVOp.tally mono
        [(GTy.lb ty1, GTy.lb ty2) ; (GTy.lb ty2, GTy.lb ty1) ;
         (GTy.ub ty1, GTy.ub ty2) ; (GTy.ub ty2, GTy.ub ty1)]
      with
      | [s'] -> s'
      | _ -> raise Exit
    in
    let t =
      try match t with
      | Let (tys, v, e1, e2) -> Let (tys, v, e1, aux ty e2)
      | Ite (e, tau, e1, e2) -> Ite (e, tau, aux ty e1, aux ty e2)
      | Projection (p, e) ->
        Projection (p, aux (GTy.map (domain_of_proj p) ty) e)
      | Constructor (cons, es) ->
        let domains_of_construct ty =
          match domains_of_construct cons ty with
          | [doms] -> doms
          | _ -> raise Exit
        in
        let tys_lb = domains_of_construct (GTy.lb ty) in
        let tys_ub = domains_of_construct (GTy.ub ty) in
        let tys = List.map2 GTy.mk_gradual tys_lb tys_ub in
        Constructor (cons, List.map2 aux tys es)
      | Lambda (da,v,e) ->
        let d = GTy.map' Arrow.domain ty in
        let cd = GTy.map2 Arrow.apply ty d in
        let ty' = GTy.mk_gradual (Arrow.mk (GTy.ub d) (GTy.lb cd)) (Arrow.mk (GTy.lb d) (GTy.ub cd)) in
        if GTy.leq ty' ty |> not then raise Exit ;
        begin match da with
        | Some da ->
          let s = unify d da in
          Lambda (Some (GTy.substitute s d), v, aux (GTy.substitute s cd) (apply_subst s e))
        | None -> Lambda (Some d, v, aux cd e)
        end
      | LambdaRec lst ->
        let n = List.length lst in
        let tys = List.mapi (fun i _ -> GTy.map (Tuple.proj n i) ty) lst in
        if GTy.leq (GTy.mapl Tuple.mk tys) ty |> not then raise Exit ;
        LambdaRec (List.combine lst tys |> List.map (fun ((tya,v,e), ty) ->
            match tya with
            | Some tya ->
              let s = unify ty tya in
              Some (GTy.substitute s ty), v, aux (GTy.substitute s ty) (apply_subst s e)
            | None -> Some ty, v, aux ty e
          ))
      | _ -> raise Exit
      with Exit -> t
    in
    let coercion_id = match coercion_id with None -> Eid.refresh id | Some id -> id in
    coercion_id, TypeCoerce ((id,t), ty, c)
  in
  aux ?coercion_id ty t

let push_coercions t =
  let f t =
    match t with
    | id, TypeCoerce (t, ty, c) -> coerce ~coercion_id:id c ty t
    | t -> t
  in
  map f t

(* ===== PRETTY PRINTER ===== *)

let pp_raw = pp

let pp_check fmt c = match c with
  | Check -> ()
  | CheckStatic -> Format.pp_print_string fmt "!"
  | NoCheck -> Format.pp_print_string fmt "!!"

let e_prio = function
  | Value _ | Var _ -> 100
  | Constructor (Tuple _, _) | Constructor (Rec _, _) | Constructor (Enum _, _) -> 100
  | Constructor (Tag _, _) -> 90
  | Constructor (Cons, _) -> 70
  | Constructor _ -> 80
  | Projection _ -> 90
  | App _ | Operation _ -> 80
  | Alt _ -> 50
  | Lambda _ | LambdaRec _ | Ite _ | Let _ | TypeCast _ | TypeCoerce _ -> 10

let rec pp fmt (_, e) = pp_e fmt e

and pp_prio prio fmt ((_, e) as t) =
  if e_prio e < prio then Format.fprintf fmt "@[(%a)@]" pp t
  else pp_e fmt e

and pp_param fmt (v,annot) =
  match annot with
  | None -> Variable.pp_uniq fmt v
  | Some dom -> Format.fprintf fmt "(%a :@ %a)" Variable.pp_uniq v GTy.pp dom

and pp_e fmt e = match e with
  | Value ts -> Format.fprintf fmt "@[<hov><%a>@]" TyScheme.pp ts
  | Var v -> Variable.pp_uniq fmt v
  | Constructor (Tuple _, []) -> Format.pp_print_string fmt "()"
  | Constructor (Tuple _, es) ->
    Format.fprintf fmt "@[<1>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        (pp_prio 0)) es
  | Constructor (Cons, [h; t]) ->
    Format.fprintf fmt "@[<hov 2>%a ::@ %a@]"
      (pp_prio 80) h (pp_prio 70) t
  | Constructor (Rec (labels, _), es) ->
    Format.fprintf fmt "@[<hv 1>{%a}@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
        (fun fmt (lbl, e) ->
          Format.fprintf fmt "@[%s =@ %a@]" lbl (pp_prio 0) e))
      (List.combine labels es)
  | Constructor (Tag tag, [e]) ->
    Format.fprintf fmt "@[%a(%a)@]" Tag.pp tag (pp_prio 0) e
  | Constructor (Enum en, []) -> Enum.pp fmt en
  | Constructor (Join _, es) ->
    Format.fprintf fmt "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ |@ ")
        (pp_prio 51)) es
  | Constructor (Meet _, es) ->
    Format.fprintf fmt "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ &@ ")
        (pp_prio 81)) es
  | Constructor (Ternary ty, [cond; e1; e2]) ->
    Format.fprintf fmt "@[<hov 2>ternary(%a,@ %a,@ %a,@ %a)@]"
      Ty.pp ty (pp_prio 0) cond (pp_prio 0) e1 (pp_prio 0) e2
  | Constructor (Normalize, [e]) ->
    Format.fprintf fmt "@[normalize(%a)@]" (pp_prio 0) e
  | Constructor (Voidify _, [e]) ->
    Format.fprintf fmt "@[void(%a)@]" (pp_prio 0) e
  | Constructor (CCustom { cname; _ }, es) ->
    Format.fprintf fmt "@[%s(%a)@]" cname
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        (pp_prio 0)) es
  | Constructor _ ->
    Format.pp_print_string fmt "<malformed-constructor>"
  | Lambda (dom, v, body) ->
    Format.fprintf fmt "@[<hov 2>fun %a ->@ %a@]"
      pp_param (v,dom) (pp_prio 0) body
  | LambdaRec [(dom, v, body)] ->
    Format.fprintf fmt "@[<hov 2>rec %a ->@ %a@]"
      pp_param (v,dom) (pp_prio 0) body
  | LambdaRec lst ->
    Format.fprintf fmt "@[<hv 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        (fun fmt (dom, v, body) ->
          Format.fprintf fmt "@[<hov 2>rec %a ->@ %a@]"
            pp_param (v,dom) (pp_prio 0) body))
      lst
  | Ite (cond, ty, e1, e2) when GTy.equiv ty (GTy.mk Ty.tt) ->
    Format.fprintf fmt "@[<hov>if %a@ then %a@ else %a@]"
      (pp_prio 0) cond (pp_prio 0) e1 (pp_prio 0) e2
  | Ite (cond, ty, e1, e2) ->
    Format.fprintf fmt "@[<hov>if %a@ is %a@ then %a@ else %a@]"
      (pp_prio 0) cond GTy.pp ty (pp_prio 0) e1 (pp_prio 0) e2
  | App (f, arg) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]"
      (pp_prio 80) f (pp_prio 90) arg
  | Operation (RecUpd lbl, (_, Constructor (Tuple 2, [record; value]))) ->
    Format.fprintf fmt "@[<hv 1>{%a@ with %s =@ %a}@]"
      (pp_prio 100) record lbl (pp_prio 0) value
  | Operation (RecUpd lbl, arg) ->
    Format.fprintf fmt "@[recupd_%s(%a)@]" lbl (pp_prio 0) arg
  | Operation (RecDel lbl, e) ->
    Format.fprintf fmt "@[%a\\%s@]" (pp_prio 90) e lbl
  | Operation (OCustom { oname; _ }, e) ->
    Format.fprintf fmt "@[<hov 2>%s@ %a@]" oname (pp_prio 90) e
  | Projection (PiField lbl, e) ->
    Format.fprintf fmt "@[%a.%s@]" (pp_prio 90) e lbl
  | Projection (PiFieldOpt lbl, e) ->
    Format.fprintf fmt "@[%a.%s?@]" (pp_prio 90) e lbl
  | Projection (Pi (2, 0), e) ->
    Format.fprintf fmt "@[fst %a@]" (pp_prio 90) e
  | Projection (Pi (2, 1), e) ->
    Format.fprintf fmt "@[snd %a@]" (pp_prio 90) e
  | Projection (Pi (_, i), e) ->
    Format.fprintf fmt "@[pi%d %a@]" i (pp_prio 90) e
  | Projection (Hd, e) ->
    Format.fprintf fmt "@[hd %a@]" (pp_prio 90) e
  | Projection (Tl, e) ->
    Format.fprintf fmt "@[tl %a@]" (pp_prio 90) e
  | Projection (PiTag tag, e) ->
    Format.fprintf fmt "@[%a.%a@]" (pp_prio 90) e Tag.pp tag
  | Projection (PCustom { pname; _ }, e) ->
    Format.fprintf fmt "@[%s %a@]" pname (pp_prio 90) e
  | Let ([], v, e1, e2) ->
    Format.fprintf fmt "@[<hov>let %a =@ %a@ in@ %a@]"
      Variable.pp_uniq v (pp_prio 0) e1 (pp_prio 0) e2
  | Let (tys, v, e1, e2) ->
    Format.fprintf fmt "@[<hov>let %a : [%a] =@ %a@ in@ %a@]"
      Variable.pp_uniq v
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        Ty.pp) tys
      (pp_prio 0) e1 (pp_prio 0) e2
  | TypeCast (e, ty, check) ->
    Format.fprintf fmt "@[(%a :%a %a)@]"
      (pp_prio 0) e pp_check check GTy.pp ty
  | TypeCoerce (e, ty, check) ->
    Format.fprintf fmt "@[(%a :>%a %a)@]"
      (pp_prio 0) e pp_check check GTy.pp ty
  | Alt (e1, e2) ->
    Format.fprintf fmt "@[<hov>%a@ |@ %a@]"
      (pp_prio 51) e1 (pp_prio 50) e2
