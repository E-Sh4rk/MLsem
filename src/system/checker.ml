open Types
open Types.Base
open Types.Tvar
open Annot
open Ast
open Env
open Parsing.Variable

(* Constants *)

let typeof_const c =
  let open Parsing.Ast in
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

(* Utils *)

let domain_of_proj p ty =
  match p with
  | Parsing.Ast.Field label ->
    mk_record true [label, (false, ty)]
  | Parsing.Ast.Pi(n,i) ->
    if i >= n then empty
    else mk_tuple (List.init n (fun k -> if k=i then ty else any))
  | Parsing.Ast.Hd ->
    mk_cons ty list_typ
  | Parsing.Ast.Tl ->
    mk_cons any (cap ty list_typ)
  | Parsing.Ast.PiTag tag ->
    mk_tag tag ty

let proj p ty =
  let open Parsing.Ast in
  match p with
  | Field label -> get_field ty label
  | Pi (n,i) -> pi n i ty
  | Hd -> destruct_list ty |> fst
  | Tl -> destruct_list ty |> snd
  | PiTag tag -> destruct_tag tag ty

(* Expressions *)

exception Untypeable of Parsing.Ast.exprid * string

let untypeable id msg = raise (Untypeable (id, msg))

let rec typeof tenv env annot (id,e) =
  let open Annot in
  match e, annot with
  | Abstract ty, AAbstract -> ty
  | Const c, AConst -> typeof_const c
  | Var v, AAx s ->
    if Env.mem v env then begin
      let (tvs, ty) = Env.find v env |> TyScheme.get in
      if TVarSet.subset (Subst.dom s) tvs then
        Subst.apply s ty
      else
        untypeable id ("Invalid substitution for "^(Variable.show v)^".")
    end else
      untypeable id ("Undefined variable "^(Variable.show v)^".")
  | Atom a, AAtom -> mk_atom a
  | Tag (tag, e), ATag annot -> mk_tag tag (typeof tenv env annot e)
  | Lambda (_, v, e), ALambda (s, annot) ->
    let env = Env.add v (TyScheme.mk_mono s) env in
    let t = typeof tenv env annot e in
    mk_arrow s t
  | Ite (e, tau, e1, e2), AIte (annot, b1, b2) ->
    let s = typeof tenv env annot e in
    if b1 = BSkip && not (subtype s (neg tau))
    then untypeable id "First branch is reachable and must be typed." ;
    if b2 = BSkip && not (subtype s tau)
    then untypeable id "Second branch is reachable and must be typed." ;
    let t1 = typeof_branch tenv env b1 e1 in
    let t2 = typeof_branch tenv env b2 e2 in
    cup t1 t2
  | App (e1, e2), AApp (annot1, annot2) ->
    let t1 = typeof tenv env annot1 e1 in
    let t2 = typeof tenv env annot2 e2 in
    if subtype t1 arrow_any then
      if subtype t2 (domain t1)
      then apply t1 t2
      else untypeable id "The argument must be in the domain of the function."
    else untypeable id "Only functions can be applied."
  | Tuple es, ATuple annots when List.length es = List.length annots ->
    List.combine es annots |> List.map (fun (e, annot) ->
      typeof tenv env annot e
    ) |> mk_tuple
  | Cons (e1, e2), ACons (annot1, annot2) ->
    let t1 = typeof tenv env annot1 e1 in
    let t2 = typeof tenv env annot2 e2 in
    if subtype t2 list_typ
    then mk_cons t1 t2
    else untypeable id "Second argument of cons must be a list."
  | Projection (p, e), AProj annot ->
    let t = typeof tenv env annot e in
    if subtype t (domain_of_proj p any)
    then proj p t
    else untypeable id "Invalid projection."
  (* TODO *)
  | _, _ -> assert false (* Expr/annot mismatch *)
and typeof_branch tenv env bannot e =
  match bannot with
  | BType annot -> typeof tenv env annot e
  | BSkip -> empty
