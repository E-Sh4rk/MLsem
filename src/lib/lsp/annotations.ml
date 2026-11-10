(* Types of variables and calls, read from a finished derivation.

   Once a definition is checked, every node the checker typed has its type
   cached in the derivation ([Annot.t]). This walks the derivation alongside
   the expression, in the order the checker types them, and reads those
   types. Nodes the checker did not type (skipped typecase branches, empty
   cells of a partitioned [Let]) have no cached type and are left out.

   Some types are cached by the reconstruction already, before a refinement
   above the node narrowed its environment; the checker keeps them as they
   are. Such a type can be wider than retyping the node would give, e.g. a
   variable's type where retyping gives [empty]. *)

open Mlsem_common
open Mlsem_types
open Mlsem_system.Ast
module Annot = Mlsem_system.Annot.Annot

type typing =
  | Use of Position.t * Variable.t * GTy.t
  (** A variable occurrence and its (instantiated) type. *)
  | Binder of Variable.t * GTy.t  (** A bound variable and the type given to it. *)
  | Call of {
      pos: Position.t;
      callee: (Variable.t * Subst.t) option;
      args: (Position.t * GTy.t) list;
      result: GTy.t;
    }
  (** A call [f a1 ... an] (curried applications flattened), with the
      callee's instance when the callee is a variable. *)

(* The sub-derivations of a node, paired with their expressions, in the order
   the checker types them. A derivation that does not match its expression is
   not walked into. *)
let children ((id, e) : Mlsem_system.Ast.t) (a : Annot.t) =
  let typed = function
    | Annot.BType a -> Some a
    | BSkip -> None
  in
  match (e, a.ann) with
  | _, (AValue _ | AVar _) -> []
  | (Constructor (_, es) | GConstructor (_, es)), AConstruct anns
    when List.length es = List.length anns ->
      List.combine es anns
  | Lambda (_, _, e), ALambda (_, a) -> [(e, a)]
  | LambdaRec lst, ALambdaRec anns when List.length lst = List.length anns ->
      List.map2 (fun (_, _, e) (_, a) -> (e, a)) lst anns
  | Ite (e, _, e1, e2), AIte (a, _, b1, b2) ->
      (e, a)
      :: List.filter_map (fun (e, b) -> Option.map (fun a -> (e, a)) (typed b)) [(e1, b1); (e2, b2)]
  | Alt (_, es), AAlt anns when List.length es = List.length anns ->
      List.combine es anns |> List.filter_map (fun (e, a) -> Option.map (fun a -> (e, a)) a)
  | App (e1, e2), AApp (a1, a2, _) -> [(e1, a1); (e2, a2)]
  | Operation (_, e), AOp (_, a, _)
  | Projection (_, e), AProj a
  | TypeCast (e, _, _), ACast (_, a)
  | TypeCoerce (e, _, _), ACoerce (_, a) ->
      [(e, a)]
  | Let (_, _, e1, e2), ALet (a1, parts) ->
      (e1, a1) :: List.filter_map (fun (_, a) -> Option.map (fun a -> (e2, a)) a) parts
  | Let (_, _, e1, e2), ALet' (a1, a2) -> [(e1, a1); (e2, a2)]
  | _, AInter anns -> List.map (fun a -> ((id, e), a)) anns
  | _ -> []

(* The typings of [expr], whose derivation is [annot], in the order the
   checker types the nodes: a node after its children. A node typed under
   several derivations of an intersection is listed once per derivation. *)
let typings expr annot =
  let acc = ref [] and inner = ref Eid.Set.empty in
  (* The callee and arguments of a call, following the function part of
     curried applications. *)
  let rec spine ((id, e) : Mlsem_system.Ast.t) (a : Annot.t) args =
    match (e, a.ann) with
    | App (e1, (id2, _)), AApp (a1, a2, _) ->
        inner := Eid.Set.add id !inner ;
        let args =
          match a2.cache with
          | Some ty -> (Eid.loc id2, ty) :: args
          | None -> args
        in
        spine e1 a1 args
    | Var v, AVar s -> (Some (v, s), args)
    (* An overloaded function part: follow its first derivation. *)
    | _, AInter (a :: _) -> spine (id, e) a args
    | _ -> (None, args)
  in
  let rec walk ((id, e) as ex : Mlsem_system.Ast.t) (a : Annot.t) =
    match a.cache with
    | None -> ()
    | Some ty -> (
        List.iter (fun (e, a) -> walk e a) (children ex a) ;
        let add t = acc := (id, t) :: !acc in
        match (e, a.ann) with
        | Var v, AVar _ -> add (Use (Eid.loc id, v, ty))
        | Lambda (_, v, _), ALambda (s, _) -> add (Binder (v, s))
        | LambdaRec lst, ALambdaRec anns when List.length lst = List.length anns ->
            List.iter2 (fun (_, v, _) (s, _) -> add (Binder (v, s))) lst anns
        | Let (_, v, _, _), (ALet (a1, _) | ALet' (a1, _)) ->
            Option.iter (fun t -> add (Binder (v, t))) a1.cache
        | App _, AApp _ ->
            let callee, args = spine ex a [] in
            inner := Eid.Set.remove id !inner ;
            add (Call {pos = Eid.loc id; callee; args; result = ty})
        | _ -> () )
  in
  walk expr annot ;
  (* Applications that are the function part of a larger call are covered by
     that call. *)
  !acc |> List.rev
  |> List.filter_map (fun (id, t) ->
    match t with
    | Call _ when Eid.Set.mem id !inner -> None
    | t -> Some t )
