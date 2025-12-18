open Base
open Tvar

module StrMap = Map.Make(String)
module VarMap = Sstt.VarMap

module TyExpr = struct
    type base =
        | TInt of Z.t option * Z.t option | TCharInt of char * char | TSString of string
        | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty | TNil
        | TString | TList | TFloat | TArrowAny | TTupleAny | TTupleN of int | TEnumAny
        | TTagAny | TRecordAny 

    type 'ext regexp =
        | Epsilon | Symbol of 'ext t
        | Union of 'ext regexp list | Concat of 'ext regexp list
        | Star of 'ext regexp | Plus of 'ext regexp | Option of 'ext regexp

    and 'ext t =
        | TVar of TVar.kind * string
        | TBase of base
        | TCustom of string
        | TApp of  string * 'ext t list
        | TEnum of string
        | TTag of string * 'ext t
        | TTuple of 'ext t list
        | TRecord of bool * (string * 'ext t * bool) list
        | TSList of 'ext regexp
        | TCons of 'ext t * 'ext t
        | TArrow of 'ext t * 'ext t
        | TCup of 'ext t * 'ext t
        | TCap of 'ext t * 'ext t
        | TDiff of 'ext t * 'ext t
        | TNeg of 'ext t
        | TWhere of 'ext t * (string * string list * 'ext t) list
        | TExt of 'ext
end

module Builder' = struct
    module type B = sig
        type ext

        exception TypeDefinitionError of string

        type type_base = TyExpr.base
        type type_regexp = ext TyExpr.regexp
        type type_expr = ext TyExpr.t

        type type_env
        type var_type_env
        val empty_tenv : type_env
        val empty_vtenv : var_type_env

        type benv = { tenv:type_env ; vtenv:var_type_env }
        val empty_benv : benv

        val type_base_to_typ : type_base -> Ty.t

        val type_expr_to_typ : benv -> type_expr -> Ty.t * benv
        val type_exprs_to_typs : benv -> type_expr list -> Ty.t list * benv

        val define_abstract : benv -> string -> int -> benv
        val define_aliases : benv -> (string * string list * type_expr) list -> benv
        val get_enum : benv -> string -> Enum.t * benv
        val get_tag : benv -> string -> Tag.t * benv

        val is_test_type : Ty.t -> bool
    end

    module type Ext = sig
        type t
        val to_typ : (t TyExpr.t -> Ty.t) -> t -> Ty.t
    end

    module Make(E:Ext) = struct
        type ext = E.t

        exception TypeDefinitionError of string

        type type_base = TyExpr.base
        type type_regexp = ext TyExpr.regexp
        type type_expr = ext TyExpr.t

        type type_env = {
            aliases : (Ty.t * TVar.t list) StrMap.t ; (* User-defined non-parametric types *)
            enums : Enum.t StrMap.t ; (* Atoms *)
            tags : Tag.t StrMap.t ; (* Tags *)
            abs : Abstract.t StrMap.t (* Abstract types *)
        }
        type var_type_env = TVar.t StrMap.t

        let empty_tenv = { aliases=StrMap.empty ; enums=StrMap.empty ;
            tags=StrMap.empty ; abs=StrMap.empty }
        let empty_vtenv = StrMap.empty
        
        type benv = { tenv:type_env ; vtenv:var_type_env }
        let empty_benv = { tenv=empty_tenv ; vtenv=empty_vtenv }

        let reg_to_sstt f r =
            let open TyExpr in
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
            let open TyExpr in
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
                let res = Subst.apply s ty in
                if List.is_empty ps |> not then
                    PEnv.register_parametrized name args res
                ; Some res
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
            | Some e -> e, tenv
            | None ->
                let e = Enum.define name in
                e, { tenv with enums=StrMap.add name e tenv.enums }
        let get_tag tenv name =
            match StrMap.find_opt name tenv.tags with
            | Some t -> t, tenv
            | None ->
                let t = Tag.define name in
                t, { tenv with tags=StrMap.add name t tenv.tags }

        let derecurse_types env defs =
            let venv =
                let h = Hashtbl.create 16 in
                StrMap.iter (fun n v -> Hashtbl.add h n v) env.vtenv ;
                h
            in
            let tenv = ref env.tenv in
            let henv = Hashtbl.create 16 in
            let eqs = ref [] in
            let rec derecurse_types defs =
                List.iter (fun (name, params, def) ->
                    Hashtbl.add henv name (def, params, [])) defs ;
                let rec get_def args name =
                    match Hashtbl.find_opt henv name with
                    | Some (def, params, lst) ->
                        let cached = lst |> List.find_opt (fun (args',_) ->
                            try List.for_all2 Ty.equiv args args' with Invalid_argument _ -> false) in
                        begin match cached with
                        | None ->
                            begin try
                                let v = TVar.mk TVar.KTemporary None in
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
                        begin match get_alias !tenv name args with
                        | Some t -> t
                        | None ->
                            begin match get_abstract_type !tenv name oargs with
                            | Some t -> t
                            | None -> raise (TypeDefinitionError (Printf.sprintf "Type %s undefined!" name))
                            end    
                        end    
                and aux lcl t =
                    let open TyExpr in
                    match t with
                    | TVar (kind,v) ->
                        begin match StrMap.find_opt v lcl, Hashtbl.find_opt venv v with
                        | Some n, _ -> n
                        | None, Some t -> TVar.typ t
                        | None, None ->
                            let t = TVar.mk kind (Some v) in
                            Hashtbl.add venv v t ;
                            TVar.typ t
                        end
                    | TBase tb -> type_base_to_typ tb
                    | TCustom n -> get_name None n
                    | TApp (n, args) ->
                        let args = args |> List.map (aux lcl) in
                        get_name (Some args) n
                    | TEnum name ->
                        let enum, tenv' = get_enum !tenv name in
                        tenv := tenv' ; Enum.typ enum
                    | TTag (name, t) ->
                        let tag, tenv' = get_tag !tenv name in
                        tenv := tenv' ; Tag.mk tag (aux lcl t)
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
                    | TExt ext -> E.to_typ (aux lcl) ext
                and aux_re lcl re =
                    re |> reg_to_sstt (aux lcl) |> Sstt.Extensions.Lists.build
                in
                let res = defs |> List.map (fun (name, params, _) ->
                    let params = List.map (fun _ -> TVar.mk KTemporary None) params in
                    let args = params |> List.map TVar.typ in
                    let node = get_def args name |> Option.get in
                    name, params, node) in
                List.iter (fun (name, _, _) -> Hashtbl.remove henv name) defs ;
                res
            in
            let res = derecurse_types defs in
            let tys = Sstt.Ty.of_eqs !eqs |> VarMap.of_list in
            let res = res |> List.map (fun (n,p,node) -> (n,p,VarMap.find node tys)) in
            let vtenv, tenv = Hashtbl.fold StrMap.add venv StrMap.empty, !tenv in
            res, { tenv ; vtenv }

        let type_expr_to_typ env t =
            match derecurse_types env [ ("", [], t) ] with
            | ([ "", [], t ], env) -> (t, env)
            | _ -> assert false

        let type_exprs_to_typs env ts =
            let env = ref env in
            let ts = List.map (fun t ->
                let (t, env') = type_expr_to_typ !env t in
                env := env' ; t
            ) ts in
            (ts, !env)

        let define_aliases env defs =
            let (res, env) = derecurse_types env defs in
            let tenv = env.tenv in
            let aliases = List.fold_left
                (fun aliases (name, params, typ) ->
                    if List.is_empty params then PEnv.register name typ ;
                    StrMap.add name (typ, params) aliases
                )
                tenv.aliases res
            in
            let tenv = { env.tenv with aliases } in
            { env with tenv }

        let define_abstract env name n =
            let tenv = env.tenv in
            if StrMap.mem name tenv.abs
            then raise (TypeDefinitionError (Printf.sprintf "Abstract type %s already defined!" name))
            else
                let tenv = { tenv with abs = StrMap.add name (Abstract.define name n) tenv.abs } in
                { env with tenv }

        let get_enum env str =
            let enum, tenv = get_enum env.tenv str in
            enum, { env with tenv }
        let get_tag env str =
            let tag, tenv = get_tag env.tenv str in
            tag, { env with tenv }

        let is_test_type t =
            let exception NotTestType in
            try
                Sstt.Ty.nodes t |> List.iter (fun t ->
                    Sstt.Ty.def t |> Sstt.VDescr.dnf |> List.iter (fun (ps,ns,d) ->
                        if ps <> [] || ns <> [] then raise NotTestType ;
                        let open Sstt.Descr in
                        components d |> fst |> List.iter (function
                            | Enums _ | Intervals _ | Tuples _ | Records _ -> ()
                            | Tags t ->
                                Tags.destruct t |> snd |> List.iter (fun comp ->
                                    let tag = Sstt.TagComp.tag comp in
                                    let ty = Sstt.Descr.mk_tagcomp comp |> Sstt.Ty.mk_descr in
                                    if Sstt.Extensions.Abstracts.is_abstract tag then
                                        let any_ty = Sstt.Extensions.Abstracts.mk_any tag in
                                        if (Ty.is_empty ty || Ty.leq any_ty ty) |> not
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
    end
end

type empty = |
module Empty = struct
    type t = empty
    let to_typ _ (t:t) = match t with | _ -> .
end
module Builder = Builder'.Make(Empty)
