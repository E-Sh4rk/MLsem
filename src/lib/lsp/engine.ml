(* The engine: a child process ([lsp.exe --engine]) that runs all mlsem work.

   The supervisor ([Server]) sends one [call] at a time over the engine's
   stdin and reads its [reply] from its stdout, both with [Marshal]; a toolkit
   call may first send [Typechecking]. Only plain
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
      known_failure: string option;
    }
  (** an [mlsem/*] toolkit request; [params] is JSON text including the
          document [text], and [known_failure] the message of the document's
          failed typecheck if the supervisor knows of one; answered with
          [Tool_result] or [Tool_refused] *)

type reply =
  | Checked of {
      lenses: Typecheck.lens list;
      diagnostics: Lsp.Types.Diagnostic.t list;
      typings: Typings.t;
    }
  | Typechecking
  (** Not an answer: the toolkit call starts typechecking its document, so
          a timeout or crash from now on is the document's. *)
  | Tool_result of string  (** JSON text *)
  | Tool_refused of string
  (** The call needed the typecheck of a document whose typecheck is
          known to fail, with that failure's message. *)

(* The toolkit requests the engine serves. *)
let tool_handlers =
  [
    ("mlsem/overloads", Handlers.overloads); ("mlsem/mergePreview", Handlers.merge_preview);
    ("mlsem/applyMerge", Handlers.apply_merge);
    ("mlsem/generalizePreview", Handlers.generalize_preview);
    ("mlsem/instantiatePreview", Handlers.instantiate_preview);
    ("mlsem/applyInstantiate", Handlers.apply_instantiate);
    ("mlsem/definitionAt", Signature_check.definition_at); ("mlsem/holes", Signature_check.holes);
    ("mlsem/definitionStatus", Signature_check.definition_status);
    ("mlsem/checkSignature", Signature_check.check_signature);
  ]

let is_tool method_ = List.mem_assoc method_ tool_handlers

(* Whether a tool needs the document's typecheck shows in what it does: it
   goes through [Store.sync_text] or not. So the handlers run unchanged, and
   [Store] refuses or reports the typecheck. *)
let handle ~typechecking = function
  | Check {uri; text} ->
      Store.known_failure := None ;
      Store.on_typecheck := ignore ;
      Store.sync_text uri text ;
      let lenses, diagnostics, typings = Store.results uri in
      Checked {lenses; diagnostics; typings}
  | Tool {method_; params; known_failure} -> (
      Store.known_failure := known_failure ;
      Store.refused := None ;
      Store.on_typecheck := typechecking ;
      let result =
        match List.assoc_opt method_ tool_handlers with
        | Some handler -> handler (Yojson.Safe.from_string params)
        | None -> `Null
      in
      match !Store.refused with
      | Some message -> Tool_refused message
      | None -> Tool_result (Yojson.Safe.to_string result) )

(* Exit as soon as the supervisor that started us is gone, even mid-typecheck.

   On Linux the kernel does it: [PR_SET_PDEATHSIG] kills us when the parent
   dies. Elsewhere a timer checks [getppid] once a second while a call is
   being handled; OCaml runs the handler at the next safe point, including
   inside long computations. While idle we block reading stdin and see EOF
   when the supervisor dies, so the timer is off then. When the parent dies
   the kernel reparents us, so [getppid] changes. *)
external set_pdeathsig : unit -> bool = "mlsem_set_pdeathsig"

let parent_gone parent = Unix.getppid () <> parent

(* Returns the functions that arm and disarm the timer around a call. *)
let exit_with_parent parent =
  if set_pdeathsig () then
    (ignore, ignore)
  else begin
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> if parent_gone parent then Unix._exit 1)) ;
    let set interval =
      ignore (Unix.setitimer Unix.ITIMER_REAL {Unix.it_interval = interval; it_value = interval})
    in
    ((fun () -> set 1.0), fun () -> set 0.0)
  end

(* The supervisor passes its pid after [--engine]; reading it from argv
   instead of [getppid] at startup catches a parent that died before we ran. *)
let parent_of_argv () =
  let rec find = function
    | "--engine" :: pid :: _ -> int_of_string_opt pid
    | _ :: rest -> find rest
    | [] -> None
  in
  find (Array.to_list Sys.argv) |> Option.value ~default:(Unix.getppid ())

let run () =
  Log.setup () ;
  let parent = parent_of_argv () in
  let arm, disarm = exit_with_parent parent in
  (* The parent may have died before the above took effect. *)
  if parent_gone parent then Unix._exit 1 ;
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
        arm () ;
        let send (reply : reply) = output_value out reply ; flush out in
        let typechecking () = send Typechecking in
        let reply = Fun.protect ~finally:disarm (fun () -> handle ~typechecking call) in
        send reply ; loop ()
  in
  loop ()
