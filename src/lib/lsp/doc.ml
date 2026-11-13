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
  (* Types of variables and calls, shifted through edits like lenses. *)
  mutable typings: Typings.t;
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
      typings = Typings.empty;
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

(* Shift typings through one edit; [true] if any range was dropped. *)
let shift_typings (t : Typings.t) shift =
  match shift with
  | Replace_all -> (Typings.empty, t <> Typings.empty)
  | Shift {s_off; e_off; delta} ->
      let dropped = ref false in
      let move (s, e) =
        match shift_offsets ~s_off ~e_off ~delta (s, e) with
        | Some r -> Some r
        | None ->
            dropped := true ;
            None
      in
      let hovers =
        List.filter_map
          (fun (h : Typings.hover) ->
             let body : Typings.hover_body =
               match h.body with
               | (Type _ | Hole _) as b -> b
               | Overloads o ->
                   (* A [val] line the edit touched loses its colour. *)
                   let row (r : Typings.overload_row) =
                     {
                       r with
                       sig_range = Option.bind r.sig_range (shift_offsets ~s_off ~e_off ~delta);
                     }
                   in
                   Overloads {o with rows = List.map row o.rows}
             in
             Option.map
               (fun (h_start, h_end) -> {Typings.h_start; h_end; body})
               (move (h.h_start, h.h_end)) )
          t.hovers
      in
      let defns =
        List.map
          (fun (d : Typings.defn) ->
             {
               Typings.tvar_occs =
                 List.filter_map
                   (fun (o : Typings.tvar_occ) ->
                      Option.map
                        (fun (o_start, o_end) -> {o with o_start; o_end})
                        (move (o.o_start, o.o_end)) )
                   d.tvar_occs;
               marks =
                 List.filter_map
                   (fun (m : Typings.mark) ->
                      Option.map
                        (fun (m_start, m_end) -> {m with m_start; m_end})
                        (move (m.m_start, m.m_end)) )
                   d.marks;
             } )
          t.defns
      in
      let calls =
        List.filter_map
          (fun (c : Typings.call) ->
             match move (c.c_start, c.c_end) with
             | None -> None
             | Some (c_start, c_end) ->
                 let args =
                   List.filter_map
                     (fun (s, e, ty) -> Option.map (fun (s, e) -> (s, e, ty)) (move (s, e)))
                     c.args
                 in
                 Some {c with c_start; c_end; args} )
          t.calls
      in
      let occs =
        List.filter_map
          (fun (o : Typings.occ) ->
             Option.map
               (fun (v_start, v_end) -> {o with v_start; v_end})
               (move (o.v_start, o.v_end)) )
          t.occs
      in
      ({hovers; defns; calls; occs}, !dropped)

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
      let typings, dropped' = shift_typings e.typings shift in
      e.lenses <- lenses ;
      e.typings <- typings ;
      if dropped || dropped' || shift = Replace_all then e.checked_digest <- no_digest ;
      e.pending <- Option.map (fun l -> shift :: l) e.pending

(* A typecheck of the current text was queued: start logging edits. *)
let start_check uri = Option.iter (fun e -> e.pending <- Some []) (find uri)

(* The pending typecheck will not report back. *)
let cancel_check uri = Option.iter (fun e -> e.pending <- None) (find uri)

(* Store a typecheck result computed from [text], shifted through the edits
   made since it was queued. *)
let set_result uri ~text ~(lenses : Typecheck.lens list) ~diagnostics ~typings =
  match find uri with
  | None -> ()
  | Some e ->
      let shifts = List.rev (Option.value e.pending ~default:[]) in
      let lenses, typings, dropped =
        List.fold_left
          (fun (lenses, typings, dropped) s ->
             let lenses, d = shift_lenses lenses s in
             let typings, d' = shift_typings typings s in
             (lenses, typings, dropped || d || d') )
          (lenses, typings, false) shifts
      in
      let replaced = List.mem Replace_all shifts in
      e.lenses <- lenses ;
      e.typings <- typings ;
      e.diagnostics <- diagnostics ;
      e.checked_digest <- (if dropped || replaced then no_digest else Digest.string text) ;
      e.pending <- None

(* Forget results, e.g. after the typecheck of this text failed. *)
let clear_results uri =
  Option.iter
    (fun e ->
       e.lenses <- [] ;
       e.diagnostics <- [] ;
       e.typings <- Typings.empty ;
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

let lsp_range e (s, e_off) =
  Lsp.Types.Range.create
    ~start:(position_of_offset e.line_offsets s)
    ~end_:(position_of_offset e.line_offsets e_off)

(* [off] lies in [[s, e]]: the end is included so that a cursor right after
   an identifier still finds it. *)
let covers off (s, e) = s <= off && off <= e

(* The hover of the tightest variable occurrence or binder at [pos]. *)
let hover uri (pos : Lsp.Types.Position.t) : (Lsp.Types.Range.t * Typings.hover_body) option =
  match find uri with
  | None -> None
  | Some e -> (
      let off = offset_of_position e.line_offsets (String.length e.text) pos in
      e.typings.hovers
      |> List.filter (fun (h : Typings.hover) -> covers off (h.h_start, h.h_end))
      |> List.sort (fun (a : Typings.hover) (b : Typings.hover) ->
        compare (a.h_end - a.h_start) (b.h_end - b.h_start) )
      |> function
      | h :: _ -> Some (lsp_range e (h.h_start, h.h_end), h.body)
      | [] -> None )

(* The resolved holes of draft [val] lines, in document order, as (range,
   hole as written, type), optionally only those overlapping [range]. *)
let holes ?range uri : (Lsp.Types.Range.t * string * string) list =
  match find uri with
  | None -> []
  | Some e ->
      let text_len = String.length e.text in
      let overlaps (s, e_off) =
        match range with
        | None -> true
        | Some (r : Lsp.Types.Range.t) ->
            let lo = offset_of_position e.line_offsets text_len r.start in
            let hi = offset_of_position e.line_offsets text_len r.end_ in
            s <= hi && lo <= e_off
      in
      e.typings.hovers
      |> List.filter_map (fun (h : Typings.hover) ->
        match h.body with
        | Hole {hole; value} when overlaps (h.h_start, h.h_end) ->
            Some (h.h_start, (lsp_range e (h.h_start, h.h_end), hole, value))
        | _ -> None )
      |> List.sort_uniq (fun (a, _) (b, _) -> compare a b)
      |> List.map snd

(* For the hover at [pos]: the parts of [val] lines its overload rows stand
   for, with the row's index. *)
let hover_overload_ranges uri pos : (int * Lsp.Types.Range.t) list =
  match (find uri, hover uri pos) with
  | Some e, Some (_, Overloads {rows; _}) ->
      List.mapi (fun i (r : Typings.overload_row) -> (i, r.sig_range)) rows
      |> List.filter_map (fun (i, r) -> Option.map (fun r -> (i, lsp_range e r)) r)
  | _ -> []

(* With the cursor on a type variable of a [val] line: that variable's
   occurrences in the definition's [val] lines, and everything in its body
   whose type mentions it. *)
let highlights uri (pos : Lsp.Types.Position.t) :
  (Lsp.Types.Range.t * [`Read | `Write | `Text]) list
  =
  match find uri with
  | None -> []
  | Some e -> (
      let off = offset_of_position e.line_offsets (String.length e.text) pos in
      let at_cursor (d : Typings.defn) =
        List.find_opt (fun (o : Typings.tvar_occ) -> covers off (o.o_start, o.o_end)) d.tvar_occs
        |> Option.map (fun (o : Typings.tvar_occ) -> (d, o.tvar))
      in
      let var_at_cursor =
        List.find_opt (fun (o : Typings.occ) -> covers off (o.v_start, o.v_end)) e.typings.occs
      in
      match (List.find_map at_cursor e.typings.defns, var_at_cursor) with
      | None, None -> []
      | None, Some o ->
          (* With the cursor on a variable: where it is bound and used. *)
          e.typings.occs
          |> List.filter (fun (o' : Typings.occ) -> o'.var = o.var)
          |> List.map (fun (o : Typings.occ) ->
            (lsp_range e (o.v_start, o.v_end), if o.binder then `Write else `Read) )
      | Some (d, tvar), _ ->
          let occs =
            d.tvar_occs
            |> List.filter (fun (o : Typings.tvar_occ) -> o.tvar = tvar)
            |> List.map (fun (o : Typings.tvar_occ) -> (o.o_start, o.o_end))
          in
          let marks =
            d.marks
            |> List.filter (fun (m : Typings.mark) -> List.mem tvar m.tvars)
            |> List.map (fun (m : Typings.mark) -> (m.m_start, m.m_end))
          in
          List.sort_uniq compare (occs @ marks) |> List.map (fun r -> (lsp_range e r, `Text)) )
