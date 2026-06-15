(* Per-document buffer + cached typecheck output.

   The store keeps the full source text so that:
   - Cached lens byte offsets can be projected back to LSP line/character
     positions against the *current* buffer.
   - Incremental [didChange] events can be applied locally so cached lenses
     follow edits without a full re-typecheck (porting [applyChangesToRange]
     from [webeditor/codelens.js]).

   Diagnostics are stored as already-projected LSP types: they are published
   on save and remain in the client until the next publish, drifting between
   edits — same caveat as the web editor. *)

module Uri = Lsp.Types.DocumentUri

(* Byte offsets of the start of each line. Always non-empty: index 0 is 0. *)
let compute_line_offsets (s : string) : int array =
  let acc = ref [0] in
  String.iteri (fun i c -> if c = '\n' then acc := (i + 1) :: !acc) s ;
  Array.of_list (List.rev !acc)

(* offset → (line, character) via binary search over [line_offsets]. *)
let position_of_offset (line_offsets : int array) (off : int) : Lsp.Types.Position.t =
  let n = Array.length line_offsets in
  let lo = ref 0 and hi = ref (n - 1) in
  while !lo < !hi do
    let mid = (!lo + !hi + 1) / 2 in
    if line_offsets.(mid) <= off then lo := mid else hi := mid - 1
  done ;
  let line = !lo in
  Lsp.Types.Position.create ~line ~character:(off - line_offsets.(line))

(* (line, character) → byte offset. Tolerates out-of-range positions by
   clamping; clients have been observed to send EOL-anchored positions. *)
let offset_of_position (line_offsets : int array) (text_len : int) (p : Lsp.Types.Position.t) : int =
  let n = Array.length line_offsets in
  let line = max 0 (min (n - 1) p.line) in
  let off = line_offsets.(line) + max 0 p.character in
  max 0 (min text_len off)

type entry = {
  mutable text: string;
  mutable line_offsets: int array;
  (* Digest of the source the cached [lenses]/[diagnostics] were computed
     from. Survives [didChange] (i.e. it is *not* the digest of [text])
     so that a save-with-no-effective-change can short-circuit even after
     edits-and-undo. *)
  mutable typechecked_digest: Digest.t;
  mutable lenses: Typecheck.lens list;
  mutable diagnostics: Lsp.Types.Diagnostic.t list;
  (* Per-binding overload data + the typecheck's final environment, kept for
     the custom merge requests. Offsets in [bindings] are shifted through
     edits alongside [lenses]; [envs] is only valid for the typechecked
     content (custom requests run on the saved buffer). *)
  mutable bindings: Typecheck.binding list;
  mutable envs: Mlsem_app.Main.envs option;
}

let entries : (Uri.t, entry) Hashtbl.t = Hashtbl.create 16

let make_entry text =
  {
    text;
    line_offsets = compute_line_offsets text;
    typechecked_digest = Digest.string "";
    lenses = [];
    diagnostics = [];
    bindings = [];
    envs = None;
  }

(* Replace any prior state for [uri] with the result of a fresh typecheck. *)
let set_result uri ~text ~(result : Typecheck.result) =
  let e = make_entry text in
  e.typechecked_digest <- Digest.string text ;
  e.lenses <- result.lenses ;
  e.diagnostics <- result.diagnostics ;
  e.bindings <- result.bindings ;
  e.envs <- result.envs ;
  Hashtbl.replace entries uri e

(* Re-typecheck the live buffer when it has drifted from the cached typecheck
   — e.g. after an applied edit that has not yet been saved. Custom requests
   call this so they act on the current text: without it a second Apply would
   still see the pre-edit state (binding still [declared = false]) and insert a
   duplicate declaration instead of replacing the one the first Apply wrote.
   Cheap when the buffer is unchanged — just a digest comparison. *)
let ensure_fresh uri =
  match Hashtbl.find_opt entries uri with
  | Some e when not (Digest.equal e.typechecked_digest (Digest.string e.text)) ->
      let result = Typecheck.run e.text in
      set_result uri ~text:e.text ~result
  | _ -> ()

(* Bring the cache up to date before a request resolves a binding, using the
   freshest text available so Apply never acts on stale bindings (which is what
   let a second Apply insert a duplicate declaration). [text] is the client's
   buffer sent with the request; [e.text] is the cache, kept current by
   didChange. We prefer the sent text when it has changed since the last
   typecheck, but fall back to the cached buffer when the sent text looks
   unchanged yet the buffer has drifted further — e.g. a stale client text
   masking the edit a previous Apply just made. *)
let sync_text uri text =
  match Hashtbl.find_opt entries uri with
  | None ->
      let result = Typecheck.run text in
      set_result uri ~text ~result
  | Some e ->
      if not (Digest.equal e.typechecked_digest (Digest.string text)) then
        let result = Typecheck.run text in
        set_result uri ~text ~result
      else if not (Digest.equal e.typechecked_digest (Digest.string e.text)) then
        let result = Typecheck.run e.text in
        set_result uri ~text:e.text ~result

(* True iff [text] hashes to the digest of the last *typechecked* content,
   regardless of intervening edits. *)
let matches_typechecked_digest uri text =
  match Hashtbl.find_opt entries uri with
  | Some e -> Digest.equal e.typechecked_digest (Digest.string text)
  | None -> false

let diagnostics uri =
  match Hashtbl.find_opt entries uri with
  | Some e -> e.diagnostics
  | None -> []

(* Indentation prefix (leading spaces/tabs) of the line containing [offset]. *)
let indent_at (text : string) (line_offsets : int array) (offset : int) : string =
  let n = Array.length line_offsets in
  let lo = ref 0 and hi = ref (n - 1) in
  while !lo < !hi do
    let mid = (!lo + !hi + 1) / 2 in
    if line_offsets.(mid) <= offset then lo := mid else hi := mid - 1
  done ;
  let start = line_offsets.(!lo) in
  let len = String.length text in
  let i = ref start in
  while !i < len && (text.[!i] = ' ' || text.[!i] = '\t') do
    incr i
  done ;
  String.sub text start (!i - start)

(* Lenses overlapping [req_range]. Returns, for each: the binder's LSP range,
   the inline-signature payload [(name, types)] (present only for typeable
   bindings — see [Typecheck.lens.signature]), and the indentation of the
   binder's source line — the code-action provider uses the indent to align
   an inserted [val] signature with the [let] binding. *)
let lenses_in_range uri (req_range : Lsp.Types.Range.t) :
  (Lsp.Types.Range.t * (string * string list) option * string) list
  =
  match Hashtbl.find_opt entries uri with
  | None -> []
  | Some e ->
      let text_len = String.length e.text in
      let req_s = offset_of_position e.line_offsets text_len req_range.start in
      let req_e = offset_of_position e.line_offsets text_len req_range.end_ in
      let lo, hi = (min req_s req_e, max req_s req_e) in
      List.filter_map
        (fun (l : Typecheck.lens) ->
           if l.end_offset < lo || l.start_offset > hi then
             None
           else
             let range =
               Lsp.Types.Range.create
                 ~start:(position_of_offset e.line_offsets l.start_offset)
                 ~end_:(position_of_offset e.line_offsets l.end_offset)
             in
             let indent = indent_at e.text e.line_offsets l.start_offset in
             Some (range, l.signature, indent) )
        e.lenses

let code_lenses uri : Lsp.Types.CodeLens.t list =
  match Hashtbl.find_opt entries uri with
  | None -> []
  | Some e ->
      List.map
        (fun (l : Typecheck.lens) ->
           let range =
             Lsp.Types.Range.create
               ~start:(position_of_offset e.line_offsets l.start_offset)
               ~end_:(position_of_offset e.line_offsets l.end_offset)
           in
           let command = Lsp.Types.Command.create ~command:"" ~title:l.title () in
           Lsp.Types.CodeLens.create ~range ~command () )
        e.lenses

let envs_of uri : Mlsem_app.Main.envs option =
  match Hashtbl.find_opt entries uri with
  | Some e -> e.envs
  | None -> None

(* The cached binding whose definition or [val] declaration span contains
   [pos], tightest span first when several nest. Used to resolve the binding
   under the cursor for the custom merge requests — so the merge view opens
   from either the [let] binder or any of its [val] declaration lines. *)
let find_binding_at uri (pos : Lsp.Types.Position.t) : Typecheck.binding option =
  match Hashtbl.find_opt entries uri with
  | None -> None
  | Some e ->
      let off = offset_of_position e.line_offsets (String.length e.text) pos in
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
        None e.bindings
      |> Option.map fst

(* Build the (range, newText) that applies [merged] as the [val name : ...]
   declaration of [b]. Replaces the existing declaration line(s) when present
   (collapsing several [val] lines into one), otherwise inserts a new line
   above the binder — mirroring [Server.inline_type_action]'s placement. *)
let merge_edit uri (b : Typecheck.binding) ~name ~merged : Lsp.Types.Range.t * string =
  let e = Hashtbl.find entries uri in
  match b.sig_offsets with
  | _ :: _ ->
      let min_start = List.fold_left (fun a (s, _) -> min a s) max_int b.sig_offsets in
      let max_end = List.fold_left (fun a (_, e) -> max a e) 0 b.sig_offsets in
      let sp = position_of_offset e.line_offsets min_start in
      let start = Lsp.Types.Position.create ~line:sp.line ~character:0 in
      let end_ = position_of_offset e.line_offsets max_end in
      let indent = indent_at e.text e.line_offsets min_start in
      let range = Lsp.Types.Range.create ~start ~end_ in
      (range, indent ^ "val " ^ name ^ " : " ^ merged)
  | [] ->
      let dp = position_of_offset e.line_offsets b.def_start in
      let insert = Lsp.Types.Position.create ~line:dp.line ~character:0 in
      let indent = indent_at e.text e.line_offsets b.def_start in
      let range = Lsp.Types.Range.create ~start:insert ~end_:insert in
      (range, indent ^ "val " ^ name ^ " : " ^ merged ^ "\n")

(* Shift a byte interval through an edit at [[s_off, e_off)] of length delta
   [delta]: intervals after the edit move by [delta], intervals before it stay,
   and intervals overlapping the edit are dropped (positions unrecoverable). *)
let shift_offsets ~s_off ~e_off ~delta (start, end_) : (int * int) option =
  if start >= e_off then
    Some (start + delta, end_ + delta)
  else if end_ <= s_off then
    Some (start, end_)
  else
    None

(* Apply one [TextDocumentContentChangeEvent] in place: update the buffer,
   recompute line offsets, and shift cached lens offsets through the edit.

   Lens shifting follows [applyChangesToRange] in [webeditor/codelens.js]:
     - lens entirely after the edit → shift by the length delta
     - lens entirely before the edit → unchanged
     - lens overlapping the edit → dropped (positions are unrecoverable)

   A change with [range = None] is a full-document replacement; we drop all
   cached lenses (we have no way to map them) and clear the typechecked
   digest so the next save re-typechecks. *)
let apply_change uri (event : Lsp.Types.TextDocumentContentChangeEvent.t) =
  match Hashtbl.find_opt entries uri with
  | None -> ()
  | Some e -> (
      match event.range with
      | None ->
          e.text <- event.text ;
          e.line_offsets <- compute_line_offsets event.text ;
          e.lenses <- [] ;
          e.bindings <- [] ;
          e.envs <- None ;
          e.typechecked_digest <- Digest.string ""
      | Some range ->
          let text_len = String.length e.text in
          let s_off = offset_of_position e.line_offsets text_len range.start in
          let e_off = offset_of_position e.line_offsets text_len range.end_ in
          let s_off, e_off = (min s_off e_off, max s_off e_off) in
          let delta = String.length event.text - (e_off - s_off) in
          let new_text =
            String.sub e.text 0 s_off ^ event.text ^ String.sub e.text e_off (text_len - e_off)
          in
          e.text <- new_text ;
          e.line_offsets <- compute_line_offsets new_text ;
          let dropped = ref false in
          e.lenses <-
            List.filter_map
              (fun (l : Typecheck.lens) ->
                 match shift_offsets ~s_off ~e_off ~delta (l.start_offset, l.end_offset) with
                 | Some (start_offset, end_offset) -> Some {l with start_offset; end_offset}
                 | None ->
                     dropped := true ;
                     None )
              e.lenses ;
          (* Shift cached bindings through the same edit so custom merge
             requests keep resolving between saves. A binding whose definition
             span overlaps the edit is dropped; individual declaration spans
             that overlap are dropped but leave the binding in place. *)
          e.bindings <-
            List.filter_map
              (fun (b : Typecheck.binding) ->
                 match shift_offsets ~s_off ~e_off ~delta (b.def_start, b.def_end) with
                 | None ->
                     dropped := true ;
                     None
                 | Some (def_start, def_end) ->
                     let sig_offsets =
                       List.filter_map (shift_offsets ~s_off ~e_off ~delta) b.sig_offsets
                     in
                     Some {b with def_start; def_end; sig_offsets} )
              e.bindings ;
          (* A dropped lens is gone for good: reverting the edit restores the
             text but not the lens. The [typechecked_digest] deliberately
             survives didChange so an edit-and-undo can short-circuit the next
             save (see the field comment) — but if any lens was dropped along
             the way, the cached set no longer matches the (restored) text, and
             that skip would serve a stale, incomplete lens set. Invalidate the
             digest so the next save re-typechecks and rebuilds the lenses. *)
          if !dropped then e.typechecked_digest <- Digest.string "" )

let remove uri = Hashtbl.remove entries uri
