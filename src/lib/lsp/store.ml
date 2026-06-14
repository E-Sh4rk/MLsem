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
}

let entries : (Uri.t, entry) Hashtbl.t = Hashtbl.create 16

let make_entry text =
  {
    text;
    line_offsets = compute_line_offsets text;
    typechecked_digest = Digest.string "";
    lenses = [];
    diagnostics = [];
  }

(* Replace any prior state for [uri] with the result of a fresh typecheck. *)
let set_result uri ~text ~(result : Typecheck.result) =
  let e = make_entry text in
  e.typechecked_digest <- Digest.string text ;
  e.lenses <- result.lenses ;
  e.diagnostics <- result.diagnostics ;
  Hashtbl.replace entries uri e

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
                 if l.start_offset >= e_off then
                   Some
                     {
                       l with
                       start_offset = l.start_offset + delta;
                       end_offset = l.end_offset + delta;
                     }
                 else if l.end_offset <= s_off then
                   Some l
                 else (
                   dropped := true ;
                   None ) )
              e.lenses ;
          (* A dropped lens is gone for good: reverting the edit restores the
             text but not the lens. The [typechecked_digest] deliberately
             survives didChange so an edit-and-undo can short-circuit the next
             save (see the field comment) — but if any lens was dropped along
             the way, the cached set no longer matches the (restored) text, and
             that skip would serve a stale, incomplete lens set. Invalidate the
             digest so the next save re-typechecks and rebuilds the lenses. *)
          if !dropped then e.typechecked_digest <- Digest.string "" )

let remove uri = Hashtbl.remove entries uri
