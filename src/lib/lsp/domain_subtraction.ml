(* The toolkit's "Domain subtraction": making the cases of selected
   overloads disjoint the way first-match pattern matching types its cases,
   each case minus the cases before it.

   A case is the first [depth] arguments of an overload, as a product: with
   [depth] 2, [A₁ -> A₂ -> R] is the case [A₁ × A₂], and [R] is kept as
   written. The difference of two products is a union of products,

     (A₁ × … × Aₙ) ∖ (B₁ × … × Bₙ)
       = ⋃ᵢ (A₁ ∧ B₁) × … × (Aᵢ₋₁ ∧ Bᵢ₋₁) × (Aᵢ ∖ Bᵢ) × Aᵢ₊₁ × … × Aₙ

   without the empty ones, so a case can become several [val] lines, one
   per product. [∧] and [∖] are written only when they change something: [a
   ∧ b] is [a] when [a ≤ b], and [a ∖ b] is [a] when they are disjoint.
   With one argument, the cases before are subtracted at once, as a union.

   The order is by case: an overload whose case is strictly inside another
   selected one's comes first, so a special case keeps its domain and the
   general case loses it ([Zero -> b] and [t -> a] give [t \ Zero -> a]).
   Otherwise the order of the [val] lines is kept.

   A case that stays one product is rewritten in place, argument by
   argument (the parser locates them); one that splits is replaced by its
   lines, printed anew. A case the earlier ones cover is unreachable; it is
   reported, not changed. A line with [where] is refused, since its types
   can use names only that line defines. *)

open Mlsem_app
open Mlsem_types
open TyExpr
open Yojson.Safe.Util

let error msg = `Assoc [("ok", `Bool false); ("error", `String msg)]

(* An argument of a case: its type as written (without locations), its
   meaning, and where it is written (its arrow and its span), [None] for one
   made by the resolution. *)
type arg = {
  expr: TyExpr.t;
  ty: Ty.t;
  at: (TyExpr.t * (int * int)) option;
}

(* The first [k] arguments of a [val] line's type, located, and the rest. *)
let rec spine k t =
  if k = 0 then
    Some ([], t)
  else
    match
      t
    with
    | TLoc (_, t) -> spine k t
    | TArrow ((TLoc ((s, e), _) as d), r) as arrow ->
        Option.map
          (fun (args, rest) -> ((d, arrow, (s.Lexing.pos_cnum, e.Lexing.pos_cnum)) :: args, rest))
          (spine (k - 1) r)
    | _ -> None

(* How many arguments a [val] line's type takes. *)
let rec arity = function
  | TLoc (_, t) -> arity t
  | TArrow (_, r) -> 1 + arity r
  | _ -> 0

let rec has_where = function
  | TLoc (_, t) -> has_where t
  | TWhere _ -> true
  | _ -> false

(* The first [k] argument types of an overload, from its meaning, so that
   its variables are the same in all of them. *)
let rec arg_types k t =
  if k = 0 then
    []
  else
    let d = Arrow.domain t in
    d :: arg_types (k - 1) (Arrow.apply t d)

let product args =
  match List.map (fun a -> a.ty) args with
  | [t] -> t
  | ts -> Tuple.mk ts

let empty args = List.exists (fun a -> Ty.is_empty a.ty) args

let meet a b =
  if Ty.leq a.ty b.ty then
    a
  else if Ty.leq b.ty a.ty then
    {b with at = None}
  else
    {expr = TCap (a.expr, b.expr); ty = Ty.cap a.ty b.ty; at = None}

let minus a b =
  if Ty.is_empty (Ty.cap a.ty b.ty) then
    a
  else
    {expr = TDiff (a.expr, b.expr); ty = Ty.diff a.ty b.ty; at = None}

(* [p ∖ q], as a union of products. *)
let diff p q =
  if Ty.is_empty (Ty.cap (product p) (product q)) then
    [p]
  else
    List.mapi
      (fun i _ ->
         List.mapi
           (fun j (a, b) ->
              if j < i then
                meet a b
              else if j = i then
                minus a b
              else
                a )
           (List.combine p q) )
      p
    |> List.filter (fun r -> not (empty r))

(* The overloads in the order their cases are taken: repeatedly, the first (in
   [val] order) of those left whose case has none of the others left
   strictly inside it. *)
let priority cases =
  let strictly_in a b = Ty.leq a b && not (Ty.leq b a) in
  let rec go left =
    match left with
    | [] -> []
    | _ ->
        let first =
          List.find (fun (_, c) -> not (List.exists (fun (_, c') -> strictly_in c' c) left)) left
        in
        first :: go (List.filter (fun x -> x != first) left)
  in
  go cases

(* Of the earlier cases overlapping one, those not inside another of them
   (the first of equal ones kept). *)
let maximal cases =
  List.filteri
    (fun i a ->
       not
         (List.exists
            (fun (j, b) ->
               let a, b = (product a, product b) in
               j <> i && Ty.leq a b && ((not (Ty.leq b a)) || j < i) )
            (List.mapi (fun j x -> (j, x)) cases) ) )
    cases

(* mlsem/subtractDomains: for the selected overloads of the binding [name],
   their cases on the first [depth] arguments made disjoint: each [val] line as it would
   be, the unreachable ones, the edits, and how many arguments all of the
   selected overloads take. *)
let subtract params : Yojson.Safe.t =
  try
    let uri =
      (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri
    in
    let text = params |> member "text" |> to_string in
    let name = params |> member "name" |> to_string in
    let wanted = params |> member "overloadTexts" |> to_list |> List.map to_string in
    let depth =
      match member "depth" params with
      | `Int d when d >= 1 -> d
      | _ -> 1
    in
    Store.sync_text uri text ;
    match (Store.find_binding_by_name uri name, Store.envs_of uri, Main.parse (`String text)) with
    | Some b, Some envs, PSuccess program -> (
        Config.restore_all () ;
        let rendered = Main.signature envs b.sigs in
        let locs = List.sort compare b.sig_offsets in
        if not (List.for_all (fun w -> List.mem w rendered) wanted) then
          `Assoc [("ok", `Bool false); ("stale", `Bool true)]
        else if (not b.declared) || List.length locs <> List.length rendered then
          error "Domain subtraction rewrites val lines: write them first, without holes."
        else
          let trees = (Typings.source program).sigs in
          let selected =
            List.combine (List.combine b.sigs rendered) locs
            |> List.filter_map (fun ((o, r), loc) ->
              if List.mem r wanted then Some (o, loc, List.assoc_opt loc trees) else None )
          in
          let max_depth =
            List.fold_left
              (fun m (_, _, t) -> min m (Option.fold ~none:0 ~some:arity t))
              max_int selected
          in
          let lines =
            List.map
              (fun (o, loc, t) ->
                 match t with
                 | Some t when has_where t ->
                     Error "A selected val line uses where: inline it first."
                 | Some t -> (
                     match spine depth t with
                     | None -> Error "A selected val line takes fewer arguments."
                     | Some (located, rest) ->
                         let tys = arg_types depth (Signature.to_gty o |> GTy.ub) in
                         let args =
                           List.map2
                             (fun (d, arrow, span) ty ->
                                {expr = Type_refactor.strip d; ty; at = Some (arrow, span)} )
                             located tys
                         in
                         Ok (loc, args, Type_refactor.strip rest) )
                 | None -> Error "A selected val line was not found." )
              selected
          in
          match
            List.find_map
              (function
                | Error e -> Some e
                | Ok _ -> None )
              lines
          with
          | Some e -> error e
          | None ->
              let lines = List.filter_map Result.to_option lines in
              let order =
                priority (List.map (fun ((_, args, _) as l) -> (l, product args)) lines)
              in
              (* Each line after the earlier ones: the products it becomes,
                 if any of them overlaps it. *)
              let results, _ =
                List.fold_left
                  (fun (results, earlier) (((loc, args, rest) as _l), _) ->
                     let overlapping =
                       List.filter
                         (fun q -> Ty.non_empty (Ty.cap (product args) (product q)))
                         earlier
                       |> maximal
                     in
                     let earlier = earlier @ [args] in
                     match overlapping with
                     | [] -> (results, earlier)
                     | q :: qs ->
                         let products =
                           if depth = 1 then (* All at once, as a union. *)
                             let union =
                               List.fold_left
                                 (fun u q ->
                                    let a = List.hd q in
                                    {expr = TCup (u.expr, a.expr); ty = Ty.cup u.ty a.ty; at = None} )
                                 (List.hd q) qs
                             in
                             diff args [union]
                           else
                             List.fold_left
                               (fun ps q -> List.concat_map (fun p -> diff p q) ps)
                               [args] overlapping
                         in
                         (results @ [(loc, args, rest, products)], earlier) )
                  ([], []) order
              in
              let offsets = Doc.compute_line_offsets text in
              let pos = Doc.position_of_offset offsets in
              let range (s, e) = Lsp.Types.Range.create ~start:(pos s) ~end_:(pos e) in
              let val_line t = "val " ^ name ^ " : " ^ Type_refactor.print t in
              (* Per changed line, its edits, as offsets in the text. *)
              let edits =
                List.filter_map
                  (fun (((s, _) as loc), args, rest, products) ->
                     match products with
                     | [] -> None
                     | [p] -> (
                         (* In place: the arguments it changes. *)
                         let changed =
                           List.filter_map
                             (fun (a, original) ->
                                match (a.at, original.at) with
                                | None, Some (arrow, span) ->
                                    Some (span, Type_refactor.print_as_child arrow 0 a.expr)
                                | _ -> None )
                             (List.combine p args)
                         in
                         match changed with
                         | [] -> None
                         | _ -> Some (loc, changed) )
                     | ps ->
                         let indent = String.make (s - offsets.((pos s).line)) ' ' in
                         let line p =
                           val_line (List.fold_right (fun a t -> TArrow (a.expr, t)) p rest)
                         in
                         Some (loc, [(loc, String.concat ("\n" ^ indent) (List.map line ps))]) )
                  results
              in
              let unreachable =
                List.filter_map
                  (fun ((s, e), _, _, products) ->
                     if products = [] then Some (String.sub text s (e - s)) else None )
                  results
              in
              (* Every [val] line of the binding, in order, as it would be. *)
              let after =
                List.map
                  (fun ((s, e) as loc) ->
                     let es =
                       Option.value (List.assoc_opt loc edits) ~default:[]
                       |> List.sort (fun ((a, _), _) ((b, _), _) -> compare b a)
                     in
                     List.fold_left
                       (fun line ((ds, de), w) ->
                          String.sub line 0 (ds - s)
                          ^ w
                          ^ String.sub line (de - s) (String.length line - (de - s)) )
                       (String.sub text s (e - s))
                       es )
                  locs
              in
              `Assoc
                [
                  ("ok", `Bool true); ("lines", `List (List.map (fun l -> `String l) after));
                  ("changed", `Int (List.length edits)); ("depth", `Int depth);
                  ("maxDepth", `Int (max 1 max_depth));
                  ("unreachable", `List (List.map (fun l -> `String l) unreachable));
                  ( "edits",
                    `List
                      (List.concat_map
                         (fun (_, es) ->
                            List.map
                              (fun (span, w) ->
                                 `Assoc
                                   [
                                     ("range", Lsp.Types.Range.yojson_of_t (range span));
                                     ("newText", `String w);
                                   ] )
                              es )
                         edits ) );
                ] )
    | _ -> error "No binding found."
  with
  | e -> error (Printexc.to_string e)
