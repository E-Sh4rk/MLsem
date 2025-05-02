open Base
open Tvar

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)
module LabelMap = Sstt.LabelMap
module VarMap = Sstt.VarMap

exception TypeDefinitionError of string

(* Construction of types *)

type type_base =
    | TInt of Z.t option * Z.t option | TCharInt of char * char | TSString of string
    | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty | TNil
    | TString | TList | TFloat | TArrowAny | TTupleAny | TTupleN of int | TAtomAny
    | TTagAny | TRecordAny 

type type_regexp = type_expr Sstt.Extensions.Lists.regexp

and type_expr =
    | TVar of string | TVarWeak of string
    | TBase of type_base
    | TCustom of string
    | TApp of  string * type_expr list
    | TAtom of string
    | TTag of string * type_expr
    | TTuple of type_expr list
    | TRecord of bool * (string * type_expr * bool) list
    | TSList of type_regexp
    | TCons of type_expr * type_expr
    | TArrow of type_expr * type_expr
    | TCup of type_expr * type_expr
    | TCap of type_expr * type_expr
    | TDiff of type_expr * type_expr
    | TNeg of type_expr
    | TWhere of type_expr * (string * string list * type_expr) list

type type_env = {
    aliases : (typ * TVar.t list) StrMap.t ; (* User-defined non-parametric types *)
    mutable atoms : atom StrMap.t ; (* Atoms *)
    mutable tags : tag StrMap.t ; (* Tags *)
    abs : abstract StrMap.t (* Abstract types *)
}
type var_type_env = TVar.t StrMap.t (* Var types *)

let empty_tenv = { aliases=StrMap.empty ; atoms=StrMap.empty ;
    tags=StrMap.empty ; abs=StrMap.empty }
let empty_vtenv = StrMap.empty

let type_base_to_typ t =
    match t with
    | TInt (lb,ub) -> interval lb ub
    | TFloat -> float_typ
    | TCharInt (c1, c2) -> char_interval c1 c2
    | TSString str -> single_string str
    | TBool -> bool_typ | TNil -> nil_typ
    | TTrue -> true_typ | TFalse -> false_typ
    | TUnit -> unit_typ | TChar -> char_typ
    | TAny -> any | TEmpty -> empty
    | TString -> string_typ | TList -> list_typ
    | TArrowAny -> arrow_any
    | TTupleAny -> tuple_any
    | TTupleN n -> tuple_n n
    | TTagAny -> tag_any
    | TAtomAny -> atom_any
    | TRecordAny -> record_any

let get_alias tenv name args =
    match StrMap.find_opt name tenv.aliases with
    | None -> None
    | Some (ty, ps) when List.length ps = List.length args ->
        let s = List.combine ps args |> Subst.construct in
        Some (Subst.apply s ty)
    | Some _ -> None
let get_abstract_type tenv name otys =
    match StrMap.find_opt name tenv.abs with
    | None -> None
    | Some abs ->
        begin match otys with
        | None -> Some (mk_abstract_any abs)
        | Some tys -> Some (mk_abstract abs tys)
        end
let get_atom tenv name =
    match StrMap.find_opt name tenv.atoms with
    | Some a -> a
    | None ->
        let a = define_atom name in
        tenv.atoms <- StrMap.add name a tenv.atoms ;
        a
let get_tag tenv name =
    match StrMap.find_opt name tenv.tags with
    | Some t -> t
    | None ->
        let t = define_tag name in
        tenv.tags <- StrMap.add name t tenv.tags ;
        t

let derecurse_types tenv venv defs =
    let venv =
        let h = Hashtbl.create 16 in
        StrMap.iter (fun n v -> Hashtbl.add h n v) venv ;
        h
    in
    let henv = Hashtbl.create 16 in
    let eqs = ref [] in
    let rec derecurse_types defs =
        List.iter (fun (name, params, def) ->
            Hashtbl.add henv name (def, params, [])) defs ;
        let rec get_def args name =
            match Hashtbl.find_opt henv name with
            | Some (def, params, lst) ->
                let cached = lst |> List.find_opt (fun (args',_) ->
                    try List.for_all2 (==) args args' with Invalid_argument _ -> false) in
                begin match cached with
                | None ->
                    begin try
                        let v = TVar.mk None in
                        Hashtbl.replace henv name (def, params, (args, v)::lst);
                        let local = List.combine params args |> List.to_seq |> StrMap.of_seq in
                        let t = aux local def in
                        eqs := (v,t)::!eqs ;
                        Some v
                    with Invalid_argument _ ->
                        raise (TypeDefinitionError (Printf.sprintf "Wrong arity for type %s!" name))
                    end
                | Some (_, v) -> Some v
                end
            | None -> None
        and get_name oargs name =
            let args = match oargs with None -> [] | Some args -> args in
            match get_def args name with
            | Some v -> TVar.typ v
            | None ->
                begin match get_alias tenv name args with
                | Some t -> t
                | None ->
                    begin match get_abstract_type tenv name oargs with
                    | Some t -> t
                    | None -> raise (TypeDefinitionError (Printf.sprintf "Type %s undefined!" name))
                    end    
                end    
        and aux lcl t =
            match t with
            | TVar v ->
                begin match StrMap.find_opt v lcl, Hashtbl.find_opt venv v with
                | Some n, _ -> n
                | None, Some t -> TVar.typ t
                | None, None ->
                    let t = TVar.mk ~user:true (Some v) in
                    Hashtbl.add venv v t ;
                    TVar.typ t
                end
            | TVarWeak v ->
                begin match Hashtbl.find_opt venv v with
                | Some t -> TVar.typ t
                | None ->
                    let t = TVar.mk ~user:false (Some v) in
                    Hashtbl.add venv v t ;
                    TVar.typ t
                end
            | TBase tb -> type_base_to_typ tb
            | TCustom n -> get_name None n
            | TApp (n, args) ->
                let args = args |> List.map (aux lcl) in
                get_name (Some args) n
            | TAtom name -> get_atom tenv name |> mk_atom
            | TTag (name, t) -> mk_tag (get_tag tenv name) (aux lcl t)
            | TTuple ts -> mk_tuple (List.map (aux lcl) ts)
            | TRecord (is_open, fields) ->
                let aux' (label,t,opt) = (label, (opt, aux lcl t)) in
                mk_record is_open (List.map aux' fields)
            | TSList lst -> aux_re lcl lst
            | TCons (t1,t2) -> mk_cons (aux lcl t1) (aux lcl t2)
            | TArrow (t1,t2) -> mk_arrow (aux lcl t1) (aux lcl t2)
            | TCup (t1,t2) ->
                let t1 = aux lcl t1 in
                let t2 = aux lcl t2 in
                cup t1 t2
            | TCap (t1,t2) ->
                let t1 = aux lcl t1 in
                let t2 = aux lcl t2 in
                cap t1 t2
            | TDiff (t1,t2) ->
                let t1 = aux lcl t1 in
                let t2 = aux lcl t2 in
                diff t1 t2
            | TNeg t -> neg (aux lcl t)
            | TWhere (t, defs) ->
                begin match derecurse_types (("", [], t)::defs) with
                | ("", [], n)::_ -> TVar.typ n
                | _ -> assert false
                end
        and aux_re lcl re =
            let open Sstt.Extensions.Lists in
            let rec aux_re re =
                match re with
                | Epsilon -> Epsilon
                | Symbol ty -> Symbol (aux lcl ty)
                | Concat lst -> Concat (List.map aux_re lst)
                | Union lst -> Union (List.map aux_re lst)
                | Star r -> Star (aux_re r)
                | Option r -> Option (aux_re r)
                | Plus r -> Plus (aux_re r)
            in
            aux_re re |> build
        in
        let res = defs |> List.map (fun (name, params, _) ->
            let params = List.map (fun _ -> TVar.mk None) params in
            let args = params |> List.map TVar.typ in
            let node = get_def args name |> Option.get in
            name, params, node) in
        List.iter (fun (name, _, _) -> Hashtbl.remove henv name) defs ;
        res
    in
    let res = derecurse_types defs in
    let tys = Sstt.Ty.of_eqs !eqs |> VarMap.of_list in
    let res = res |> List.map (fun (n,p,node) -> (n,p,VarMap.find node tys)) in
    let venv = Hashtbl.fold StrMap.add venv StrMap.empty in
    (res, venv)

let type_expr_to_typ tenv venv t =
    match derecurse_types tenv venv [ ("", [], t) ] with
    | ([ "", [], t ], venv) -> (t, venv)
    | _ -> assert false

let type_exprs_to_typs env venv ts =
    let venv = ref venv in
    let ts = List.map (fun t ->
        let (t, venv') = type_expr_to_typ env !venv t in
        venv := venv' ; t
    ) ts in
    (ts, !venv)

let define_types tenv venv defs =
    let (res, _) = derecurse_types tenv venv defs in
    let aliases = List.fold_left
        (fun aliases (name, params, typ) ->
            if params = [] then register name typ ;
            StrMap.add name (typ, params) aliases
        )
        tenv.aliases res
    in
    { tenv with aliases }

let define_abstract tenv name vs =
    if StrMap.mem name tenv.abs
    then raise (TypeDefinitionError (Printf.sprintf "Abstract type %s already defined!" name))
    else { tenv with abs = StrMap.add name (define_abstract name vs) tenv.abs }

(* Operations on types *)

let is_test_type t =
    let exception NotTestType in
    try
        Sstt.Ty.nodes t |> List.iter (fun t ->
            Sstt.Ty.def t |> Sstt.VDescr.dnf |> List.iter (fun (ps,ns,d) ->
                if ps <> [] || ns <> [] then raise NotTestType ;
                let open Sstt.Descr in
                components d |> List.iter (function
                    | Atoms _ | Intervals _ | Tuples _ | Records _ -> ()
                    | Tags t ->
                        Tags.destruct t |> snd |> List.iter (fun comp ->
                            let tag = Sstt.TagComp.tag comp in
                            let ty = Sstt.Descr.mk_tagcomp comp |> Sstt.Ty.mk_descr in
                            let any_ty = Sstt.Extensions.Abstracts.mk_any tag in
                            if Sstt.Extensions.Abstracts.is_abstract tag &&
                                (is_empty ty || subtype any_ty ty) |> not
                            then raise NotTestType
                        )
                    | Arrows a ->
                        let t = mk_arrows a |> Sstt.Ty.mk_descr in
                        if (is_empty t || subtype arrow_any t) |> not
                        then raise NotTestType
                )
            )
        ) ; true
    with NotTestType -> false

let branch_type lst =
    if lst = [] then arrow_any
    else begin
        lst
        |> List.map (fun (a, b) -> mk_arrow a b)
        |> conj
    end
let tuple_branch_type ts = mk_tuple ts
let cons_branch_type (a, b) = mk_cons a b
let record_branch_type (fields, o) = mk_record o fields

(* Simplification of types *)

let simplify_typ = Sstt.Transform.simplify

(* Record manipulation *)

let record_any_with l = mk_record true [l, (false, any)]

let record_any_without l = mk_record true [l, (true, empty)]

let remove_field_info t label =
    let t = remove_field t label in
    let singleton = mk_record false [label, (true, any)] in
    merge_records t singleton

(* Operations on type variables *)

let instantiate ss t =
    List.map (fun s -> Subst.apply s t) ss
    |> conj

let bot_instance mono =
    clean ~pos:empty ~neg:any mono

let top_instance mono =
    clean ~pos:any ~neg:empty mono
