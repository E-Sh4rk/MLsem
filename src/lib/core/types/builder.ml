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
    | TString | TList | TFloat | TArrowAny | TTupleAny | TTupleN of int | TEnumAny
    | TTagAny | TRecordAny 

type type_regexp =
    | Epsilon | Symbol of type_expr 
    | Union of type_regexp list | Concat of type_regexp list
    | Star of type_regexp | Plus of type_regexp | Option of type_regexp

and type_expr =
    | TVar of string | TVarWeak of string
    | TBase of type_base
    | TCustom of string
    | TApp of  string * type_expr list
    | TEnum of string
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
    aliases : (Ty.t * TVar.t list) StrMap.t ; (* User-defined non-parametric types *)
    mutable enums : Enum.t StrMap.t ; (* Atoms *)
    mutable tags : Tag.t StrMap.t ; (* Tags *)
    abs : Abstract.t StrMap.t (* Abstract types *)
}
type var_type_env = TVar.t StrMap.t (* Var types *)

let empty_tenv = { aliases=StrMap.empty ; enums=StrMap.empty ;
    tags=StrMap.empty ; abs=StrMap.empty }
let empty_vtenv = StrMap.empty

let reg_to_sstt f r =
    let open Sstt.Extensions in
    let rec aux r =
        match r with
        | Epsilon -> Lists.Epsilon
        | Symbol s -> Lists.Symbol (f s)
        | Union u -> Lists.Union (List.map aux u)
        | Concat c -> Lists.Concat (List.map aux c)
        | Star s -> Lists.Star (aux s)
        | Plus p -> Lists.Plus (aux p)
        | Option o -> Lists.Option (aux o)
    in aux r

let type_base_to_typ t =
    match t with
    | TInt (lb,ub) -> Ty.interval lb ub
    | TFloat -> Ty.float
    | TCharInt (c1, c2) -> Ty.char_interval c1 c2
    | TSString str -> Ty.string_lit str
    | TBool -> Ty.bool | TNil -> Lst.nil
    | TTrue -> Ty.tt | TFalse -> Ty.ff
    | TUnit -> Ty.unit | TChar -> Ty.char
    | TAny -> Ty.any | TEmpty -> Ty.empty
    | TString -> Ty.string | TList -> Lst.any
    | TArrowAny -> Arrow.any
    | TTupleAny -> Tuple.any
    | TTupleN n -> Tuple.any_n n
    | TTagAny -> Tag.any
    | TEnumAny -> Enum.any
    | TRecordAny -> Record.any

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
        | None -> Some (Abstract.any abs)
        | Some tys -> Some (Abstract.mk abs tys)
        end
let get_enum tenv name =
    match StrMap.find_opt name tenv.enums with
    | Some e -> e
    | None ->
        let e = Enum.define name in
        tenv.enums <- StrMap.add name e tenv.enums ;
        e
let get_tag tenv name =
    match StrMap.find_opt name tenv.tags with
    | Some t -> t
    | None ->
        let t = Tag.define name in
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
            | TEnum name -> get_enum tenv name |> Enum.typ
            | TTag (name, t) -> Tag.mk (get_tag tenv name) (aux lcl t)
            | TTuple ts -> Tuple.mk (List.map (aux lcl) ts)
            | TRecord (is_open, fields) ->
                let aux' (label,t,opt) = (label, (opt, aux lcl t)) in
                Record.mk is_open (List.map aux' fields)
            | TSList lst -> aux_re lcl lst
            | TCons (t1,t2) -> Lst.cons (aux lcl t1) (aux lcl t2)
            | TArrow (t1,t2) -> Arrow.mk (aux lcl t1) (aux lcl t2)
            | TCup (t1,t2) ->
                let t1 = aux lcl t1 in
                let t2 = aux lcl t2 in
                Ty.cup t1 t2
            | TCap (t1,t2) ->
                let t1 = aux lcl t1 in
                let t2 = aux lcl t2 in
                Ty.cap t1 t2
            | TDiff (t1,t2) ->
                let t1 = aux lcl t1 in
                let t2 = aux lcl t2 in
                Ty.diff t1 t2
            | TNeg t -> Ty.neg (aux lcl t)
            | TWhere (t, defs) ->
                begin match derecurse_types (("", [], t)::defs) with
                | ("", [], n)::_ -> TVar.typ n
                | _ -> assert false
                end
        and aux_re lcl re =
            re |> reg_to_sstt (aux lcl) |> Sstt.Extensions.Lists.build
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
            if params = [] then Ty.register name typ ;
            StrMap.add name (typ, params) aliases
        )
        tenv.aliases res
    in
    { tenv with aliases }

let define_abstract tenv name vs =
    if StrMap.mem name tenv.abs
    then raise (TypeDefinitionError (Printf.sprintf "Abstract type %s already defined!" name))
    else { tenv with abs = StrMap.add name (Abstract.define name vs) tenv.abs }

let is_test_type t =
    let exception NotTestType in
    try
        Sstt.Ty.nodes t |> List.iter (fun t ->
            Sstt.Ty.def t |> Sstt.VDescr.dnf |> List.iter (fun (ps,ns,d) ->
                if ps <> [] || ns <> [] then raise NotTestType ;
                let open Sstt.Descr in
                components d |> List.iter (function
                    | Enums _ | Intervals _ | Tuples _ | Records _ -> ()
                    | Tags t ->
                        Tags.destruct t |> snd |> List.iter (fun comp ->
                            let tag = Sstt.TagComp.tag comp in
                            let ty = Sstt.Descr.mk_tagcomp comp |> Sstt.Ty.mk_descr in
                            let any_ty = Sstt.Extensions.Abstracts.mk_any tag in
                            if Sstt.Extensions.Abstracts.is_abstract tag &&
                                (Ty.is_empty ty || Ty.leq any_ty ty) |> not
                            then raise NotTestType
                        )
                    | Arrows a ->
                        let t = mk_arrows a |> Sstt.Ty.mk_descr in
                        if (Ty.is_empty t || Ty.leq Arrow.any t) |> not
                        then raise NotTestType
                )
            )
        ) ; true
    with NotTestType -> false
