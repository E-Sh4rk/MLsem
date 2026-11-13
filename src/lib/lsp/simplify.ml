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
   - all: the last three, in the order MLsem applies them, then printed.

   Results that are the same are offered once, with their labels together;
   a result that is the part as written is not offered. *)

open Mlsem_app
open Mlsem_types
open TyExpr

let parse s =
  match Type_refactor.strip (IO.parse_type_string s) with
  | t -> Some t
  | exception _ -> None

(* The simplified forms of the part [p] of the [val] line [line], as written
   in its place, with the steps that give each. *)
let options envs line (p : Type_refactor.part) =
  let benv, _, _, _, _ = envs in
  let build t = Signature.build benv t |> fst |> Signature.to_gty |> GTy.ub in
  match p.node with
  | TOption _ -> None
  | _ -> (
      match build p.node with
      | exception _ -> None
      | ty ->
          (* The values of the line's variables that occur one way only, by
             name: the part's variables are built apart from the line's. *)
          let unused =
            match build line with
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
          let label ls =
            if List.length ls = List.length steps then "any step" else String.concat " · " ls
          in
          Some (List.map (fun (e, ls) -> (Type_parts.written p e, Some (label ls))) results) )

(* mlsem/simplify: the [val] lines of the definition at [position] with the
   parts that simplify and their simplified forms ([Type_parts.lines]). *)
let parts params : Yojson.Safe.t = Type_parts.lines params options
