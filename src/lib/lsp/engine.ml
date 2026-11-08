(* The engine: a child process ([lsp.exe --engine]) that runs all mlsem work.

   The supervisor ([Server]) sends one [call] at a time over the engine's
   stdin and reads one [reply] from its stdout, both with [Marshal]. Only plain
   data crosses: texts, offsets, strings, LSP diagnostics, JSON as text. mlsem
   types stay here, because they carry ids from process-global counters that
   mean nothing in another process.

   The engine is the unit the supervisor kills on a timeout or cancellation.
   It keeps its typecheck cache across calls but needs no state from outside:
   every call carries the document text, so a fresh engine just recomputes. *)

type call =
  | Check of {
      uri: Lsp.Types.DocumentUri.t;
      text: string;
    }  (** typecheck [text], answered with [Checked] *)
  | Tool of {
      method_: string;
      params: string;
    }
  (** an [mlsem/*] toolkit request; [params] is JSON text including the
          document [text]; answered with [Tool_result] *)

type reply =
  | Checked of {
      lenses: Typecheck.lens list;
      diagnostics: Lsp.Types.Diagnostic.t list;
    }
  | Tool_result of string  (** JSON text *)

(* The toolkit requests the engine serves. *)
let tool_handlers =
  [
    ("mlsem/overloads", Handlers.overloads); ("mlsem/mergePreview", Handlers.merge_preview);
    ("mlsem/applyMerge", Handlers.apply_merge);
    ("mlsem/instantiatePreview", Handlers.instantiate_preview);
    ("mlsem/applyInstantiate", Handlers.apply_instantiate);
  ]

let is_tool method_ = List.mem_assoc method_ tool_handlers

let handle = function
  | Check {uri; text} ->
      Store.sync_text uri text ;
      let lenses, diagnostics = Store.results uri in
      Checked {lenses; diagnostics}
  | Tool {method_; params} ->
      let result =
        match List.assoc_opt method_ tool_handlers with
        | Some handler -> handler (Yojson.Safe.from_string params)
        | None -> `Null
      in
      Tool_result (Yojson.Safe.to_string result)

(* Exit as soon as the supervisor that started us is gone, even mid-typecheck.
   A timer signal fires every second; OCaml runs the handler at the next safe
   point, including inside long computations. The handler only checks and
   exits, it never interrupts work otherwise. When the parent dies the kernel
   reparents us, so [getppid] changes. *)
let exit_with_parent () =
  let parent = Unix.getppid () in
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ -> if Unix.getppid () <> parent then Unix._exit 1)) ;
  ignore (Unix.setitimer Unix.ITIMER_REAL {Unix.it_interval = 1.0; it_value = 1.0})

let run () =
  Log.setup () ;
  exit_with_parent () ;
  (* Replies go to a private copy of stdout, and fd 1 is pointed at stderr,
     so nothing else printed by mlsem can corrupt the protocol. *)
  let out = Unix.out_channel_of_descr (Unix.dup Unix.stdout) in
  Unix.dup2 Unix.stderr Unix.stdout ;
  set_binary_mode_in stdin true ;
  set_binary_mode_out out true ;
  Log.Server.info (fun m -> m "engine started") ;
  let rec loop () =
    match (input_value stdin : call) with
    | exception End_of_file -> exit 0
    | call ->
        output_value out (handle call) ;
        flush out ;
        loop ()
  in
  loop ()
