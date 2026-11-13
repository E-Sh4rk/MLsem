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

(* A named binding retained so that custom requests can re-render its
   overloads and merge a chosen subset on demand: a typeable one, or a
   declared one whose definition does not typecheck, with the overloads of
   its [val] lines ([checked] false), which the tools that only rewrite
   types can still work on. [sigs] is the raw overload list (the merge
   input); rendering and merging need the final [envs] kept on [result].
   Offsets are byte intervals, shifted through edits by
   [Store.apply_change] just like [lens] offsets. *)
type binding = {
  name: string;
  declared: bool;
  checked: bool;
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
  (* Types of variables and calls, for hover and highlighting. *)
  typings: Typings.t;
}

let empty = {lenses = []; diagnostics = []; bindings = []; envs = None; typings = Typings.empty}

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

(* The result of a [##] debug query (the only [Message]), on one line. The
   printer puts each tally solution in brackets over several lines; they are
   joined, with "or" between solutions. A tally without solution prints
   nothing. *)
let one_line_result title descr =
  let lines =
    Option.value descr ~default:"" |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (( <> ) "")
  in
  let rec join prev = function
    | [] -> []
    | l :: ls when prev = "]" && String.starts_with ~prefix:"[" l -> "or" :: l :: join l ls
    | l :: ls -> l :: join l ls
  in
  match lines with
  | [] -> title ^ ": no solution"
  | lines -> title ^ ": " ^ String.concat " " (join "" lines)

let diagnostic ?(severity = LT.DiagnosticSeverity.Error) ?tags ~range ~message () =
  LT.Diagnostic.create ~message:(`String message) ~range ~severity ?tags ~source:"mlsem" ()

let make_lens ?signature ~start_offset ~end_offset ~title () =
  {start_offset; end_offset; title; signature}

let add_message acc (sev, pos, title, descr, kind) =
  let range =
    match lsp_range_of_pos pos with
    | Some r -> r
    | None -> fallback_range ()
  in
  (* Code no derivation reaches (a typecase branch skipped in all of them):
     the editor fades it out. *)
  let tags =
    match (kind : Mlsem_system.Analyzer.Diagnostic.kind option) with
    | Some Unreachable -> Some [LT.DiagnosticTag.Unnecessary]
    | Some (Unchecked _)
    | None ->
        None
  in
  let message =
    match (sev : Mlsem_system.Analyzer.severity) with
    | Message -> one_line_result title descr
    | Notice
    | Warning
    | Error ->
        full_message title descr
  in
  diagnostic ~severity:(severity_of sev) ?tags ~range ~message () :: acc

(* Where the line holding byte [off] of [source] starts. *)
let line_start source off =
  match String.rindex_from_opt source (off - 1) '\n' with
  | Some i -> i + 1
  | None -> 0
  | exception Invalid_argument _ -> 0

(* For a call [f a] that fails, where [f] is a definition typed earlier
   ([bindings]), changes of [f]'s [val] lines that may make the call
   typecheck, written as drafts so the definition's check gives the result:
   - add the overload [<type of a> -> _];
   - widen the domain of an overload with [a]'s type: [<domain> | <type of a> -> _].
   Nothing more is checked: whether the definition still typechecks against
   them, and whether the call then does, shows once applied.
   Only a failure of [f]'s first argument is handled, and only an argument
   type without type variables.
   A definition stops at its first failure, so a definition calling [f]
   twice wrongly ([let g x = (f true, f "a")]) shows only the first call:
   the second appears once the first is fixed. *)
let app_fixes envs source val_types bindings (err : Mlsem_lang.Error.t) : Typings.fix list =
  let open Mlsem_types in
  let open TyExpr in
  match (err.kind, err.expr) with
  | UntypeableApp {arg; _}, App ((_, Var f), _) when TVOp.is_ground_typ arg -> (
      let callee =
        match (offsets_of_pos (Variable.get_location f), Variable.get_name f) with
        | Some (s, e), Some name ->
            List.find_opt
              (fun (b : binding) -> b.name = name && b.def_start = s && b.def_end = e)
              bindings
        | _ -> None
      in
      match (offsets_of_pos (Eid.loc err.eid), callee) with
      | Some (x_start, x_end), Some b -> (
          let parse s =
            match IO.parse_type_string s with
            | t -> Some t
            | exception _ -> None
          in
          let rendered = Main.signature envs b.sigs in
          let locs = List.sort compare b.sig_offsets in
          (* Each overload's type as a tree: its [val] line as written when
             each overload is one line, else as printed. *)
          let overload_types =
            if b.declared && List.length locs = List.length rendered then
              List.map (fun loc -> List.assoc_opt loc val_types) locs
            else
              List.map parse rendered
          in
          let arg_expr = parse (Main.display_gty envs (GTy.mk arg)) in
          let hole = TVar (KInfer, "_hole") in
          (* [t] with its domain widened with [a] and its result a hole. *)
          let rec widen_domain a = function
            | TLoc (_, t) -> widen_domain a t
            | TWhere (t, defs) -> Option.map (fun t -> TWhere (t, defs)) (widen_domain a t)
            | TArrow (d, _) -> Some (TArrow (TCup (d, a), hole))
            | _ -> None
          in
          let print t = Type_refactor.checked_print (Type_refactor.strip t) in
          (* The draft [dom -> _]. *)
          let draft dom = print (TArrow (dom, hole)) in
          let val_line ty = "val " ^ b.name ^ " : " ^ ty in
          let at_binder = line_start source b.def_start in
          (* Add [decl] as a [val] line; a definition without [val] lines also
             gets lines for its current type, which the new one would
             otherwise replace. *)
          let lines decls = String.concat "" (List.map (fun d -> val_line d ^ "\n") decls) in
          let add decl =
            [(at_binder, at_binder, lines ((if b.declared then [] else rendered) @ [decl]))]
          in
          (* Replace the [k]th overload by [decl]: its [val] line when each
             overload is one line; else (a draft line can give several) all
             the lines are rewritten, the first replaced and the others
             deleted. *)
          let replace k decl =
            let decls = List.mapi (fun i r -> if i = k then decl else r) rendered in
            if List.length locs = List.length rendered then
              let s, e = List.nth locs k in
              [(s, e, val_line decl)]
            else
              match
                locs
              with
              | [] -> [(at_binder, at_binder, lines decls)]
              | (s, e) :: rest ->
                  let next_line e =
                    match String.index_from_opt source e '\n' with
                    | Some i -> i + 1
                    | None -> String.length source
                  in
                  (s, e, String.concat "\n" (List.map val_line decls))
                  :: List.map (fun (s, e) -> (line_start source s, next_line e, "")) rest
          in
          let fix title edits = {Typings.x_start; x_end; title; edits} in
          match arg_expr with
          | None -> []
          | Some a ->
              let added =
                match draft a with
                | Some d -> [fix ("Add overload: " ^ val_line d) (add d)]
                | None -> []
              in
              (* Not for an overload whose domain has the argument already:
                 the call fails for another reason. *)
              let widened =
                List.mapi
                  (fun k (o, t) ->
                     let dom = Arrow.domain (Signature.to_gty o |> GTy.ub) in
                     if Ty.leq arg dom then
                       None
                     else
                       Option.bind t (fun t ->
                         Option.bind (widen_domain a t) (fun t ->
                           Option.map
                             (fun d -> fix ("Widen domain: " ^ val_line d) (replace k d))
                             (print t) ) ) )
                  (List.combine b.sigs overload_types)
                |> List.filter_map Fun.id
              in
              added @ widened )
      | _ -> [] )
  | _ -> []

let add_result ?typings ?(source = "") envs acc (out : Main.output) : result =
  ( match (out.res, typings) with
  | TSuccess (lst, _), Some ctx -> List.iter (Typings.add ctx envs) lst
  | TFailure (_, _, _, _, _, Some err), Some ctx ->
      Typings.add_fixes ctx (app_fixes envs source ctx.src.sigs acc.bindings err)
  | _ -> () ) ;
  match out.res with
  (* Statements without bindings, e.g. the result of a [##] debug query. *)
  | TDone -> {acc with diagnostics = List.fold_left add_message acc.diagnostics out.msg}
  | TFailure (v_opt, pos, msg, descr, _time, _) ->
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
      (* A declared definition keeps its overloads, from its [val] lines: they
         were all built before any definition was checked. *)
      let bindings =
        let _, _, senv, _, _ = envs in
        match v_opt with
        | Some v -> (
            match
              ( Variable.get_name v,
                offsets_of_pos (Variable.get_location v),
                Variable.get_sig_locations v,
                VarMap.find_opt v senv )
            with
            | Some name, Some (def_start, def_end), (_ :: _ as locs), Some sigs
              when not (List.exists (fun (b : binding) -> b.name = name) acc.bindings) ->
                {
                  name;
                  declared = true;
                  checked = false;
                  def_start;
                  def_end;
                  sig_offsets = List.filter_map offsets_of_pos locs;
                  sigs;
                }
                :: acc.bindings
            | _ -> acc.bindings )
        | None -> acc.bindings
      in
      {acc with lenses; diagnostics = diag :: acc.diagnostics; bindings}
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
                     {
                       name;
                       declared = b.declared;
                       checked = true;
                       def_start;
                       def_end;
                       sig_offsets;
                       sigs = b.sigs;
                     }
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
          let typings = Typings.create program in
          let final_envs, acc =
            List.fold_left
              (fun (env, acc) e ->
                 let env, res = treat_def env e in
                 (env, add_result ~typings ~source env acc res) )
              (envs, acc) program
          in
          (* Keep the final environment so custom requests can render and
             merge the cached overloads in the same elaboration context. *)
          {acc with envs = Some final_envs; typings = Typings.finish typings}
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
