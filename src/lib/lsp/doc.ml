(* The supervisor's per-document state: plain data only.

   For each open document we keep the current text and the last typecheck's
   lenses and diagnostics. CodeLens and codeAction are answered from here
   straight away, without the engine. Lenses carry byte offsets, shifted
   through each [didChange] so they follow edits without a re-typecheck
   (porting [applyChangesToRange] from [webeditor/codelens.js]).

   A typecheck runs on a snapshot of the text while the user may keep
   editing. From the moment it is queued we log the edits; when its result
   arrives, the result's lenses are shifted through the logged edits so they
   match the text as it is by then.

   Diagnostics are stored as already-projected LSP types: they are published
   once per typecheck and drift between edits, same caveat as the web
   editor. *)

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

(* One edit as it affects lens offsets. *)
type shift =
  | Shift of {
      s_off: int;
      e_off: int;
      delta: int;
    }
  | Replace_all  (** full-document replacement: no offset survives *)

type entry = {
  mutable text: string;
  mutable line_offsets: int array;
  (* Digest of the text the cached [lenses]/[diagnostics] were computed from.
     Survives [didChange] (it is *not* the digest of [text]) so that a save
     with no effective change can skip the typecheck even after
     edits-and-undo. *)
  mutable checked_digest: Digest.t;
  mutable lenses: Typecheck.lens list;
  mutable diagnostics: Lsp.Types.Diagnostic.t list;
  (* Edits since a typecheck was queued, newest first; [None] when no
     typecheck is pending. *)
  mutable pending: shift list option;
}

let entries : (Uri.t, entry) Hashtbl.t = Hashtbl.create 16
let no_digest = Digest.string ""
let find uri = Hashtbl.find_opt entries uri

let open_ uri text =
  Hashtbl.replace entries uri
    {
      text;
      line_offsets = compute_line_offsets text;
      checked_digest = no_digest;
      lenses = [];
      diagnostics = [];
      pending = None;
    }

let close uri = Hashtbl.remove entries uri
let text uri = Option.map (fun e -> e.text) (find uri)

(* Shift lenses through one edit; [true] if any lens was dropped. *)
let shift_lenses lenses shift =
  match shift with
  | Replace_all -> ([], lenses <> [])
  | Shift {s_off; e_off; delta} ->
      let dropped = ref false in
      let lenses =
        List.filter_map
          (fun (l : Typecheck.lens) ->
             match shift_offsets ~s_off ~e_off ~delta (l.start_offset, l.end_offset) with
             | Some (start_offset, end_offset) -> Some {l with start_offset; end_offset}
             | None ->
                 dropped := true ;
                 None )
          lenses
      in
      (lenses, !dropped)

(* Apply one [TextDocumentContentChangeEvent]: update the buffer, shift the
   cached lenses through the edit, and log the edit for a pending typecheck.

   A lens overlapping the edit is gone for good: reverting the edit restores
   the text but not the lens. So when one is dropped the [checked_digest] is
   cleared, otherwise a later save of the restored text would skip the
   typecheck and keep serving an incomplete lens set. *)
let apply_change uri (event : Lsp.Types.TextDocumentContentChangeEvent.t) =
  match find uri with
  | None -> ()
  | Some e ->
      let shift =
        match event.range with
        | None ->
            e.text <- event.text ;
            Replace_all
        | Some range ->
            let text_len = String.length e.text in
            let s_off = offset_of_position e.line_offsets text_len range.start in
            let e_off = offset_of_position e.line_offsets text_len range.end_ in
            let s_off, e_off = (min s_off e_off, max s_off e_off) in
            let delta = String.length event.text - (e_off - s_off) in
            e.text <-
              String.sub e.text 0 s_off ^ event.text ^ String.sub e.text e_off (text_len - e_off) ;
            Shift {s_off; e_off; delta}
      in
      e.line_offsets <- compute_line_offsets e.text ;
      let lenses, dropped = shift_lenses e.lenses shift in
      e.lenses <- lenses ;
      if dropped || shift = Replace_all then e.checked_digest <- no_digest ;
      e.pending <- Option.map (fun l -> shift :: l) e.pending

(* A typecheck of the current text was queued: start logging edits. *)
let start_check uri = Option.iter (fun e -> e.pending <- Some []) (find uri)

(* The pending typecheck will not report back. *)
let cancel_check uri = Option.iter (fun e -> e.pending <- None) (find uri)

(* Store a typecheck result computed from [text], shifted through the edits
   made since it was queued. *)
let set_result uri ~text ~(lenses : Typecheck.lens list) ~diagnostics =
  match find uri with
  | None -> ()
  | Some e ->
      let shifts = List.rev (Option.value e.pending ~default:[]) in
      let lenses, dropped =
        List.fold_left
          (fun (lenses, dropped) s ->
             let lenses, d = shift_lenses lenses s in
             (lenses, dropped || d) )
          (lenses, false) shifts
      in
      let replaced = List.mem Replace_all shifts in
      e.lenses <- lenses ;
      e.diagnostics <- diagnostics ;
      e.checked_digest <- (if dropped || replaced then no_digest else Digest.string text) ;
      e.pending <- None

(* Forget results, e.g. after the typecheck of this text failed. *)
let clear_results uri =
  Option.iter
    (fun e ->
       e.lenses <- [] ;
       e.diagnostics <- [] ;
       e.checked_digest <- no_digest ;
       e.pending <- None )
    (find uri)

(* True iff the cached results were computed from [text] and are complete,
   so a typecheck of [text] would change nothing. *)
let up_to_date uri text =
  match find uri with
  | Some e -> e.pending = None && Digest.equal e.checked_digest (Digest.string text)
  | None -> false

let diagnostics uri =
  match find uri with
  | Some e -> e.diagnostics
  | None -> []

(* Lenses overlapping [req_range]. Returns, for each: the binder's LSP range,
   the inline-signature payload [(name, types)] (present only for typeable
   bindings — see [Typecheck.lens.signature]), and the indentation of the
   binder's source line — the code-action provider uses the indent to align
   an inserted [val] signature with the [let] binding. *)
let lenses_in_range uri (req_range : Lsp.Types.Range.t) :
  (Lsp.Types.Range.t * (string * string list) option * string) list
  =
  match find uri with
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
  match find uri with
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
