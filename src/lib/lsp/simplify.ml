(* The toolkit's "Simplify": rewriting parts of [val] lines' types with the
   steps MLsem simplifies a signature with, each on its own and all at once.
   Every step keeps the meaning of the line, and each result is printed by
   MLsem's printer, which is a step of its own: it prints the type from its
   meaning, so a union of a type and a subtype becomes the type ([int |
   digit] is [int]) and declared types are recognized by name.

   The steps:
   - printed: only that;
   - regrouped: [Ty.simplify], which merges the lines of the type's normal
     form and removes redundant ones (sstt's [Transform.simplify]);
   - factorized: [Ty.factorize], which takes out what the lines of the
     normal form share;
   - unused variables dropped: a variable of the line that occurs only in
     output positions of the whole line becomes [empty], and one only in
     input positions [any] ([TyScheme.bot_instance], as when MLsem
     simplifies a signature). The positions are the line's, not the part's;
   - all: the last three, in the order MLsem applies them, then printed;
   - unused definitions dropped: a [where] without the definitions its body
     does not use, the rest as written;
   - declared type: a declared type equal to the part, by name;
   - variables merged: for the whole line only, two of its variables made
     one when that means the same ([merged_variables]).

   Results that are the same are offered once, with their labels together;
   a result that is the part as written is not offered. *)

open Mlsem_app
open Mlsem_types
open TyExpr

let parse s =
  match Type_refactor.strip (IO.parse_type_string s) with
  | t -> Some t
  | exception _ -> None

(* Whether [t1] is a subtype of an instance of [t2], [t2]'s weak variables
   instantiable and every other variable fixed: what [## t1 <= t2] checks
   ([Main.poly_leq], which [Main] does not export). *)
let poly_leq t1 t2 =
  let mono = TVOp.all_vars KNoInfer in
  if MVarSet.subset (TVOp.vars t1) mono && MVarSet.subset (TVOp.vars t2) mono then
    Ty.leq t1 t2
  else
    TVOp.tally mono [(t1, t2)] <> []

(* The line [line_ty] with two of its variables made one, when that means
   the same: the merged type is an instance of the line (one variable put
   for the other), and the line must be an instance of it too, that is an
   instance of the merged type, its variables instantiable, below the line
   ([tree('a) -> int -> 'a -> tree('a)] for [tree('b) -> int -> 'a ->
   tree('b | 'a)], with ['a := 'b | 'a], as [tree] is covariant). One
   option per pair of variables that merge, the first by name kept; not on
   a line with holes or weak variables. Each with its label. *)
let merged_variables envs line line_ty =
  (* Built as [Typings.build_signature] does, and with [envs] whose printer
     knows the instances of declared types met building it: [tree('a)] is
     printed by name only if it was built somewhere. *)
  let build t =
    let benv, varm, senv, env, penv = envs in
    let (s, _), penv = PEnv.sequential_handler penv (Signature.build benv) t in
    (Signature.to_gty s |> GTy.ub, (benv, varm, senv, env, penv))
  in
  let line = Type_refactor.strip line in
  let vars =
    Type_refactor.subterms line
    |> List.filter_map (function
      | TVar (KNoInfer, n) -> Some n
      | _ -> None )
    |> List.sort_uniq compare
  in
  let weak =
    List.exists
      (function
        | TVar (KInfer, _) -> true
        | _ -> false )
      (Type_refactor.subterms line)
  in
  let rec pairs = function
    | [] -> []
    | a :: rest -> List.map (fun b -> (a, b)) rest @ pairs rest
  in
  let rec rename b a t =
    match t with
    | TVar (KNoInfer, n) when n = b -> TVar (KNoInfer, a)
    | t -> Type_refactor.map_children (rename b a) t
  in
  if weak then
    []
  else
    pairs vars
    |> List.filter_map (fun (a, b) ->
      match build (rename b a line) with
      | exception _ -> None
      | merged, envs ->
          let instance = Subst.apply (TVOp.refresh ~kind:KInfer (TVOp.vars merged)) merged in
          if poly_leq instance line_ty then
            Some
              ( Printf.sprintf "variables '%s and '%s merged" a b,
                Main.display_gty envs (GTy.mk (merged |> Ty.simplify |> Ty.factorize)) )
          else
            None )

(* The simplified forms of the part [p] of the [val] line [line], as written
   in its place, with the steps that give each. *)
let options envs line (p : Type_refactor.part) =
  let build = Type_parts.build envs line in
  match p.node with
  | TOption _ -> None
  | _ -> (
      match build p.node with
      | exception _ -> None
      | ty ->
          (* The values of the line's variables that occur one way only, by
             name: the part's variables are built apart from the line's. *)
          let unused =
            match Typings.build_signature envs line |> fst |> Signature.to_gty |> GTy.ub with
            | exception _ -> []
            | line_ty ->
                TVOp.vars_with_polarity1 line_ty
                |> List.filter_map (fun (v, pol) ->
                  match pol with
                  | `Pos -> Some (TVar.name v, Ty.empty)
                  | `Neg -> Some (TVar.name v, Ty.any)
                  | `Both -> None )
          in
          let drop t =
            TVOp.vars t |> MVarSet.proj1 |> TVarSet.elements
            |> List.filter_map (fun v ->
              Option.map (fun ty -> (v, ty)) (List.assoc_opt (TVar.name v) unused) )
            |> Subst.of_list1
            |> fun s -> Subst.apply s t
          in
          let steps =
            [
              ("printed", ty); ("regrouped", Ty.simplify ty); ("factorized", Ty.factorize ty);
              ("unused variables dropped", drop ty);
              ("all", ty |> drop |> Ty.simplify |> Ty.factorize);
            ]
          in
          (* A part using the line's [where] names is printed with their
             definitions again, which says nothing new: only its declared
             types are offered. *)
          let steps =
            match Typings.build_signature envs p.node with
            | _ -> steps
            | exception _ -> []
          in
          (* Compared up to the order of unions and intersections, which the
             printer may change without simplifying anything; a duplicate
             member it drops is a simplification. *)
          let same = Type_refactor.same_but_order in
          let original = Type_refactor.strip p.node in
          let results =
            List.fold_left
              (fun acc (label, t) ->
                 match parse (Main.display_gty envs (GTy.mk t)) with
                 | None -> acc
                 | Some e when same e original -> acc
                 | Some e when List.exists (fun (e', _) -> same e e') acc ->
                     List.map
                       (fun (e', ls) -> if same e e' then (e', ls @ [label]) else (e', ls))
                       acc
                 | Some e -> acc @ [(e, [label])] )
              [] steps
          in
          (* A declared type that is the part: its name. *)
          let results =
            Type_parts.declared envs build ty
            |> List.fold_left
                 (fun acc c ->
                    match build c with
                    | c_ty
                      when Ty.leq c_ty ty && Ty.leq ty c_ty
                           && (not (same c original))
                           && not (List.exists (fun (e, _) -> same c e) acc) ->
                        acc @ [(c, ["declared type"])]
                    | _ -> acc
                    | exception _ -> acc )
                 results
          in
          (* The part without the [where] definitions nothing uses, as
             written otherwise. *)
          let results =
            match Type_refactor.drop_unused p.node with
            | None -> results
            | Some e ->
                let l = "unused definitions dropped" in
                if List.exists (fun (e', _) -> same e e') results then
                  List.map (fun (e', ls) -> if same e e' then (e', ls @ [l]) else (e', ls)) results
                else
                  results @ [(e, [l])]
          in
          (* The whole line with two variables merged, printed as the steps
             are. *)
          let results =
            if p.parent <> None || steps = [] then
              results
            else
              List.fold_left
                (fun acc (label, printed) ->
                   match parse printed with
                   | None -> acc
                   | Some e when same e original -> acc
                   | Some e when List.exists (fun (e', _) -> same e e') acc ->
                       List.map
                         (fun (e', ls) -> if same e e' then (e', ls @ [label]) else (e', ls))
                         acc
                   | Some e -> acc @ [(e, [label])] )
                results (merged_variables envs p.node ty)
          in
          let label ls =
            if steps <> [] && List.for_all (fun (l, _) -> List.mem l ls) steps then
              "any step"
            else
              String.concat " · " ls
          in
          Some (List.map (fun (e, ls) -> (Type_parts.written p e, Some (label ls))) results) )

(* mlsem/simplify: the [val] lines of the definition at [position] with the
   parts that simplify and their simplified forms ([Type_parts.lines]). *)
let parts params : Yojson.Safe.t = Type_parts.lines params options
