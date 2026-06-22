(* Runs the MLsem typechecker on a source buffer and converts its results
   into intermediate, position-stable values.

   Mirrors the logic of [src/bin/web.ml], but targets LSP clients instead of
   the web editor. Each top-level binding becomes one CodeLens descriptor
   anchored on the binder's source location. The descriptors carry byte
   offsets rather than line/character positions so that [Store] can shift
   them through edits without re-typechecking — see the [applyChangesToRange]
   approach in [webeditor/codelens.js]. Diagnostics are emitted as LSP types
   directly: we publish them once per typecheck and don't try to keep them
   accurate during edits. *)

open Mlsem_common
open Mlsem_app
open Mlsem_app.Main
module LT = Lsp.Types

(* A lens range over the typechecked source, expressed as a byte interval.
   [Store] converts these to [Lsp.Types.Range.t] at codeLens response time
   using the current buffer's line offsets, after possibly shifting them
   through edits. *)
type lens = {
  start_offset: int;
  end_offset: int;
  title: string;
  (* For a successfully-typed binding, the pieces needed to synthesize a
     [val name : type] signature: the binder name and the binding's signature(s)
     rendered as valid [val] surface syntax (a binding may carry several
     overload signatures). [None] for untypeable bindings, where no
     inline-signature action is offered. *)
  signature: (string * string list) option;
}

(* A typeable, named binding retained so that custom requests can re-render
   its overloads and merge a chosen subset on demand. [sigs] is the raw
   overload list (the merge input); rendering and merging need the final
   [envs] kept on [result]. Offsets are byte intervals, shifted through edits
   by [Store.apply_change] just like [lens] offsets. *)
type binding = {
  name: string;
  declared: bool;
  def_start: int;
  def_end: int;
  (* Source spans of any existing [val name : ...] declaration lines, used to
     replace them on apply. Empty for inferred (undeclared) bindings. *)
  sig_offsets: (int * int) list;
  sigs: Signature.t;
}

type result = {
  lenses: lens list;
  diagnostics: LT.Diagnostic.t list;
  (* Per-binding overload data for custom merge requests. *)
  bindings: binding list;
  (* Final environment of the typecheck, needed to render/merge overloads.
     [None] when the source failed to parse or sigs failed to elaborate. *)
  envs: Main.envs option;
}

let empty = {lenses = []; diagnostics = []; bindings = []; envs = None}

(* LSP positions are 0-indexed; Lexing.pos_lnum is 1-indexed while
   [Position.column] already returns a 0-indexed offset within the line. *)
let lsp_position (p : Lexing.position) : LT.Position.t =
  LT.Position.create ~line:(max 0 (Position.line p - 1)) ~character:(max 0 (Position.column p))

let lsp_range_of_pos (pos : Position.t) : LT.Range.t option =
  if pos = Position.dummy then
    None
  else
    let start = lsp_position (Position.start_of_position pos) in
    let end_ = lsp_position (Position.end_of_position pos) in
    Some (LT.Range.create ~start ~end_)

let offsets_of_pos (pos : Position.t) : (int * int) option =
  if pos = Position.dummy then
    None
  else
    Some
      ( Position.offset (Position.start_of_position pos),
        Position.offset (Position.end_of_position pos) )

(* Fallback range when a diagnostic has no usable source position. Placing
   it at (0,0) keeps it visible in the problems panel rather than dropping
   it silently. *)
let fallback_range () =
  let z = LT.Position.create ~line:0 ~character:0 in
  LT.Range.create ~start:z ~end_:z

let severity_of (s : Mlsem_system.Analyzer.severity) : LT.DiagnosticSeverity.t =
  match s with
  | Error -> LT.DiagnosticSeverity.Error
  | Warning -> LT.DiagnosticSeverity.Warning
  | Notice -> LT.DiagnosticSeverity.Information
  | Message -> LT.DiagnosticSeverity.Hint

let full_message title descr =
  match descr with
  | None -> title
  | Some d -> title ^ ":\n" ^ d

let diagnostic ?(severity = LT.DiagnosticSeverity.Error) ~range ~message () =
  LT.Diagnostic.create ~message:(`String message) ~range ~severity ~source:"mlsem" ()

let make_lens ?signature ~start_offset ~end_offset ~title () =
  {start_offset; end_offset; title; signature}

let add_message acc (sev, pos, title, descr) =
  let range =
    match lsp_range_of_pos pos with
    | Some r -> r
    | None -> fallback_range ()
  in
  diagnostic ~severity:(severity_of sev) ~range ~message:(full_message title descr) () :: acc

let add_result envs acc (out : Main.output) : result =
  match out.res with
  | TDone -> acc
  | TFailure (v_opt, pos, msg, descr, _time) ->
      let def_pos =
        match v_opt with
        | Some v -> Variable.get_location v
        | None -> pos
      in
      let lenses =
        match offsets_of_pos def_pos with
        | None -> acc.lenses
        | Some (s, e) ->
            make_lens ~start_offset:s ~end_offset:e ~title:("Untypeable: " ^ msg) () :: acc.lenses
      in
      let diag_range =
        match lsp_range_of_pos pos with
        | Some r -> r
        | None -> (
            match lsp_range_of_pos def_pos with
            | Some r -> r
            | None -> fallback_range () )
      in
      let diag =
        diagnostic ~severity:LT.DiagnosticSeverity.Error ~range:diag_range
          ~message:(full_message msg descr) ()
      in
      {acc with lenses; diagnostics = diag :: acc.diagnostics}
  | TSuccess (lst, _time) ->
      let lenses =
        List.fold_left
          (fun lenses (b : inferred) ->
             match offsets_of_pos (Variable.get_location b.var) with
             | None -> lenses
             | Some (s, e) ->
                 let name = Variable.get_name b.var |> Option.value ~default:"_" in
                 (* [val name : type] inline action, offered only for
                    genuinely-named bindings that lack a user-written
                    declaration. *)
                 let signature =
                   if b.declared || name = "" || name = "_" then
                     None
                   else
                     Some (name, Main.signature envs b.sigs)
                 in
                 (* Lens title keeps the [∀]-quantified display form. *)
                 make_lens ?signature ~start_offset:s ~end_offset:e
                   ~title:(name ^ " : " ^ Main.display envs b.ty)
                   ()
                 :: lenses )
          acc.lenses lst
      in
      let diagnostics = List.fold_left add_message acc.diagnostics out.msg in
      (* Retain every genuinely-named binding (declared ones included, so a
         declared binding can be re-merged) for the custom merge requests. *)
      let bindings =
        List.fold_left
          (fun bindings (b : inferred) ->
             match Variable.get_name b.var with
             | None
             | Some ""
             | Some "_" ->
                 bindings
             | Some name -> (
                 match offsets_of_pos (Variable.get_location b.var) with
                 | None -> bindings
                 | Some (def_start, def_end) ->
                     let sig_offsets =
                       Variable.get_sig_locations b.var |> List.filter_map offsets_of_pos
                     in
                     {name; declared = b.declared; def_start; def_end; sig_offsets; sigs = b.sigs}
                     :: bindings ) )
          acc.bindings lst
      in
      {acc with lenses; diagnostics; bindings}

(* Counterpart to [web.ml]'s [typecheck]: runs the full pipeline on the
   source string. Returns a parse/internal error as a single diagnostic
   (with no lenses) or the accumulated per-binding results. *)
let run (source : string) : result =
  Config.restore_all () ;
  try
    match parse (`String source) with
    | PFailure (pos, msg) ->
        let range =
          match lsp_range_of_pos pos with
          | Some r -> r
          | None -> fallback_range ()
        in
        {
          empty with
          diagnostics = [diagnostic ~severity:LT.DiagnosticSeverity.Error ~range ~message:msg ()];
        }
    | PSuccess program ->
        let envs, sigs_out = treat_all_sigs initial_envs program in
        let acc = add_result envs empty sigs_out in
        let sigs_ok =
          match sigs_out.res with
          | TFailure _ -> false
          | _ -> true
        in
        if not sigs_ok then
          acc
        else
          let final_envs, acc =
            List.fold_left
              (fun (env, acc) e ->
                 let env, res = treat_def env e in
                 (env, add_result env acc res) )
              (envs, acc) program
          in
          (* Keep the final environment so custom requests can render and
             merge the cached overloads in the same elaboration context. *)
          {acc with envs = Some final_envs}
  with
  | e ->
      {
        empty with
        diagnostics =
          [
            diagnostic ~range:(fallback_range ())
              ~message:("internal error: " ^ Printexc.to_string e)
              ();
          ];
      }
