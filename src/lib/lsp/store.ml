(* The engine's per-document cache of typecheck results.

   Every engine call carries the document's text. [sync_text] typechecks it
   unless it is what this cache already holds, so the cache is always the
   result for exactly that text: binding offsets and the typechecked
   environment ([envs]) match the request's positions. A fresh engine starts
   empty and rebuilds on demand.

   [envs] and [bindings] hold mlsem types, which only make sense inside this
   process: they never leave the engine. *)

module Uri = Lsp.Types.DocumentUri

type entry = {
  text: string;
  line_offsets: int array;
  digest: Digest.t;
  result: Typecheck.result;
}

let entries : (Uri.t, entry) Hashtbl.t = Hashtbl.create 16

(* Make [text] the cached content of [uri], typechecking it unless it already
   is. *)
let sync_text uri text =
  let digest = Digest.string text in
  match Hashtbl.find_opt entries uri with
  | Some e when Digest.equal e.digest digest -> ()
  | _ ->
      let result = Typecheck.run text in
      Hashtbl.replace entries uri
        {text; line_offsets = Doc.compute_line_offsets text; digest; result}

(* Lenses and diagnostics of the cached typecheck. *)
let results uri =
  match Hashtbl.find_opt entries uri with
  | Some e -> (e.result.lenses, e.result.diagnostics)
  | None -> ([], [])

let envs_of uri : Mlsem_app.Main.envs option =
  Option.bind (Hashtbl.find_opt entries uri) (fun e -> e.result.envs)

(* The cached binding whose definition or [val] declaration span contains
   [pos], tightest span first when several nest. Used to resolve the binding
   under the cursor for the custom merge requests — so the merge view opens
   from either the [let] binder or any of its [val] declaration lines. *)
let find_binding_at uri (pos : Lsp.Types.Position.t) : Typecheck.binding option =
  match Hashtbl.find_opt entries uri with
  | None -> None
  | Some e ->
      let off = Doc.offset_of_position e.line_offsets (String.length e.text) pos in
      (* Width of the tightest of the binding's spans (definition + each [val]
         declaration) that contains [off], or [None] if [off] is in none. *)
      let containing_width (b : Typecheck.binding) : int option =
        (b.def_start, b.def_end) :: b.sig_offsets
        |> List.filter_map (fun (s, en) -> if s <= off && off <= en then Some (en - s) else None)
        |> function
        | [] -> None
        | ws -> Some (List.fold_left min max_int ws)
      in
      List.fold_left
        (fun best (b : Typecheck.binding) ->
           match containing_width b with
           | None -> best
           | Some w -> (
               match best with
               | Some (_, bw) when bw <= w -> best
               | _ -> Some (b, w) ) )
        None e.result.bindings
      |> Option.map fst

(* The cached binding with the given [name]. Binding names are unique at top
   level, so this disambiguates members of a [let .. and ..] group that share
   no positional information — preview/apply identify their target by name
   (stable across edits) rather than re-resolving a possibly-stale position. *)
let find_binding_by_name uri name : Typecheck.binding option =
  match Hashtbl.find_opt entries uri with
  | None -> None
  | Some e -> List.find_opt (fun (b : Typecheck.binding) -> b.name = name) e.result.bindings

(* The edit(s) that write [decls] as the [val name : ...] declaration line(s)
   of [b], one [val] line per element.

   [mode = `Replace] (default): with existing declaration line(s), replace the
   first with all the new lines and delete the rest as whole lines — so
   non-adjacent [val] lines are rewritten without disturbing any code between
   them. With no existing declarations, a single insert above the binder
   (mirroring [Server.inline_type_action]'s placement).

   [mode = `Insert]: never touch existing declarations; always emit the new
   [val] line(s) just above the binder. Existing declarations sit above the
   binder too, so the new ones join the group rather than replacing it. *)
let signature_edit ?(mode = `Replace) uri (b : Typecheck.binding) ~name ~decls :
  (Lsp.Types.Range.t * string) list
  =
  let e = Hashtbl.find entries uri in
  let position_of_offset = Doc.position_of_offset e.line_offsets in
  let indent_at = Doc.indent_at e.text e.line_offsets in
  let lines indent =
    decls |> List.map (fun d -> "val " ^ name ^ " : " ^ d) |> String.concat ("\n" ^ indent)
  in
  let insert_above () =
    let dp = position_of_offset b.def_start in
    let insert = Lsp.Types.Position.create ~line:dp.line ~character:0 in
    let indent = indent_at b.def_start in
    [(Lsp.Types.Range.create ~start:insert ~end_:insert, indent ^ lines indent ^ "\n")]
  in
  match (mode, List.sort (fun (a, _) (b, _) -> compare a b) b.sig_offsets) with
  | `Insert, _
  | `Replace, [] ->
      insert_above ()
  | `Replace, (first_start, first_end) :: rest ->
      (* Replace the first declaration in place. *)
      let fp = position_of_offset first_start in
      let first_line_start = Lsp.Types.Position.create ~line:fp.line ~character:0 in
      let first_end_pos = position_of_offset first_end in
      let indent = indent_at first_start in
      let replace =
        (Lsp.Types.Range.create ~start:first_line_start ~end_:first_end_pos, indent ^ lines indent)
      in
      (* Delete each remaining declaration as a whole line (up to the start of
         the next line), leaving anything in between untouched. *)
      let last_line = Array.length e.line_offsets - 1 in
      let deletions =
        List.map
          (fun (s, _) ->
             let p = position_of_offset s in
             let line_start = Lsp.Types.Position.create ~line:p.line ~character:0 in
             (* Delete the whole line up to the start of the next one; for the
                last line (no next line) end at the document end so the emitted
                range stays valid. *)
             let line_end =
               if p.line >= last_line then
                 position_of_offset (String.length e.text)
               else
                 Lsp.Types.Position.create ~line:(p.line + 1) ~character:0
             in
             (Lsp.Types.Range.create ~start:line_start ~end_:line_end, "") )
          rest
      in
      replace :: deletions
