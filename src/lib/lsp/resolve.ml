(* The toolkit's "Resolve overlap": making the domains of selected overloads
   disjoint the way first-match pattern matching types its cases, each
   domain minus the domains of the cases before it.

   The order is by domain: an overload whose domain is strictly inside
   another selected one's comes first, so a special case keeps its domain
   and the general case loses it ([Zero -> b] and [t -> a] give
   [t \ Zero -> a]). Otherwise the order of the [val] lines is kept.

   Only the domain of a changed [val] line is rewritten, in place: the
   parser locates it. An overload whose domain the earlier ones cover is
   unreachable; it is reported, not changed. A line with [where] is refused,
   since its domain can use names only that line defines. *)

open Mlsem_app
open Mlsem_types
open TyExpr
open Yojson.Safe.Util

let error msg = `Assoc [("ok", `Bool false); ("error", `String msg)]

(* The domain of a [val] line's type, if it is an arrow: the located domain
   node, the arrow (without locations around it) and the span of the
   domain. *)
let rec domain = function
  | TLoc (_, t) -> domain t
  | TArrow ((TLoc ((s, e), _) as d), _) as arrow ->
      Some (d, arrow, (s.Lexing.pos_cnum, e.Lexing.pos_cnum))
  | _ -> None

let rec has_where = function
  | TLoc (_, t) -> has_where t
  | TWhere _ -> true
  | _ -> false

(* The overloads in the order they are resolved: repeatedly, the first (in
   [val] order) of those left whose domain has none of the others left
   strictly inside it. *)
let priority doms =
  let strictly_in a b = Ty.leq a b && not (Ty.leq b a) in
  let rec go left =
    match left with
    | [] -> []
    | _ ->
        let first =
          List.find (fun (_, d) -> not (List.exists (fun (_, d') -> strictly_in d' d) left)) left
        in
        first :: go (List.filter (fun x -> x != first) left)
  in
  go doms

(* mlsem/resolveOverlap: for the selected overloads of the binding [name],
   each [val] line as it would be, the unreachable ones, and the edits. *)
let resolve params : Yojson.Safe.t =
  try
    let uri =
      (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri
    in
    let text = params |> member "text" |> to_string in
    let name = params |> member "name" |> to_string in
    let wanted = params |> member "overloadTexts" |> to_list |> List.map to_string in
    Store.sync_text uri text ;
    match (Store.find_binding_by_name uri name, Store.envs_of uri, Main.parse (`String text)) with
    | Some b, Some envs, PSuccess program -> (
        Config.restore_all () ;
        let rendered = Main.signature envs b.sigs in
        let locs = List.sort compare b.sig_offsets in
        if not (List.for_all (fun w -> List.mem w rendered) wanted) then
          `Assoc [("ok", `Bool false); ("stale", `Bool true)]
        else if (not b.declared) || List.length locs <> List.length rendered then
          error "Resolving rewrites val lines: write them first, without holes."
        else
          let trees = (Typings.source program).sigs in
          let selected =
            List.combine (List.combine b.sigs rendered) locs
            |> List.filter (fun ((_, r), _) -> List.mem r wanted)
          in
          let lines =
            List.map
              (fun ((o, _), loc) ->
                 match List.assoc_opt loc trees with
                 | Some t when has_where t ->
                     Error "A selected val line uses where: inline it first."
                 | Some t -> (
                     match domain t with
                     | Some d -> Ok (loc, Arrow.domain (Signature.to_gty o |> GTy.ub), d)
                     | None -> Error "A selected val line is not an arrow." )
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
              let order = priority (List.map (fun ((loc, dom, _) as l) -> ((loc, l), dom)) lines) in
              (* Each line after the earlier ones: its new domain, if any of
                 them overlaps it. *)
              let changes, unreachable, _ =
                List.fold_left
                  (fun (changes, unreachable, earlier) ((loc, (_, dom, (d, arrow, span))), _) ->
                     (* The earlier domains it overlaps, without those inside
                        another of them (the first of equal ones kept). *)
                     let overlapping =
                       List.filter (fun (dom', _) -> Ty.non_empty (Ty.cap dom dom')) earlier
                     in
                     let overlapping =
                       List.filteri
                         (fun i (a, _) ->
                            not
                              (List.exists
                                 (fun (j, (b, _)) ->
                                    j <> i && Ty.leq a b && ((not (Ty.leq b a)) || j < i) )
                                 (List.mapi (fun j x -> (j, x)) overlapping) ) )
                         overlapping
                     in
                     let earlier = earlier @ [(dom, d)] in
                     match overlapping with
                     | [] -> (changes, unreachable, earlier)
                     | (_, first) :: rest ->
                         let covered = Ty.disj (List.map fst overlapping) in
                         if Ty.leq dom covered then
                           (changes, loc :: unreachable, earlier)
                         else
                           let minus =
                             List.fold_left (fun acc (_, d') -> TCup (acc, d')) first rest
                           in
                           let written =
                             Type_refactor.print_as_child arrow 0
                               (Type_refactor.strip (TDiff (d, minus)))
                           in
                           ((loc, span, written) :: changes, unreachable, earlier) )
                  ([], [], []) order
              in
              let offsets = Doc.compute_line_offsets text in
              let pos = Doc.position_of_offset offsets in
              (* Every [val] line of the binding, in order, as it would be. *)
              let after =
                List.map
                  (fun ((s, e) as loc) ->
                     match List.find_opt (fun (l, _, _) -> l = loc) changes with
                     | Some (_, (ds, de), w) ->
                         String.sub text s (ds - s) ^ w ^ String.sub text de (e - de)
                     | None -> String.sub text s (e - s) )
                  locs
              in
              let line_text (s, e) = String.sub text s (e - s) in
              `Assoc
                [
                  ("ok", `Bool true); ("lines", `List (List.map (fun l -> `String l) after));
                  ("changed", `Int (List.length changes));
                  ( "unreachable",
                    `List (List.map (fun l -> `String (line_text l)) (List.rev unreachable)) );
                  ( "edits",
                    `List
                      (List.map
                         (fun (_, (ds, de), w) ->
                            `Assoc
                              [
                                ( "range",
                                  Lsp.Types.Range.yojson_of_t
                                    (Lsp.Types.Range.create ~start:(pos ds) ~end_:(pos de)) );
                                ("newText", `String w);
                              ] )
                         changes ) );
                ] )
    | _ -> error "No binding found."
  with
  | e -> error (Printexc.to_string e)
