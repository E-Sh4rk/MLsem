(* The language server the editor talks to (the supervisor).

   It never runs mlsem code itself, so it always stays responsive. It speaks
   LSP, keeps a plain copy of each open document with its last lenses and
   diagnostics ([Doc]), and answers codeLens and codeAction from that copy at
   once. Typechecks and the [mlsem/*] toolkit requests are jobs for the
   engine process ([Engine]): one at a time, the rest wait in [queue].

   On a timeout or cancellation the engine is killed (SIGKILL frees all it
   held), and a fresh one is started for the next job. A text whose job timed
   out or crashed the engine is remembered and not tried again until the text
   changes.

   One wind-down path ([wind_down]) handles every way of stopping. *)

open Jsonrpc
module Uri = Lsp.Types.DocumentUri
module J = Yojson.Safe

(* --- Wire framing over raw file descriptors ------------------------------ *)

(* Reads go through [Unix.read] on descriptors, never an [in_channel]: a
   channel can hold buffered data while [Unix.select] reports nothing ready. *)

let rec write_all fd s off len =
  if len > 0 then
    match
      Unix.write_substring fd s off len
    with
    | n -> write_all fd s (off + n) (len - n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s off len

(* Read what is available into [buf]; [false] on end of file. *)
let read_into fd buf =
  let chunk = Bytes.create 65536 in
  match Unix.read fd chunk 0 (Bytes.length chunk) with
  | 0 -> false
  | n -> Buffer.add_subbytes buf chunk 0 n ; true
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> true

(* Remove the first [n] bytes of [buf]. *)
let consume buf n =
  let rest = Buffer.sub buf n (Buffer.length buf - n) in
  Buffer.clear buf ; Buffer.add_string buf rest

(* LSP framing: [Content-Length] header, blank line, JSON body. *)
module Frame = struct
  let header_end s =
    let n = String.length s in
    let rec go i =
      if i + 3 >= n then
        None
      else if s.[i] = '\r' && s.[i + 1] = '\n' && s.[i + 2] = '\r' && s.[i + 3] = '\n' then
        Some i
      else
        go (i + 1)
    in
    go 0

  let content_length headers =
    String.split_on_char '\n' headers
    |> List.find_map (fun line ->
      match String.index_opt line ':' with
      | None -> None
      | Some i ->
          let key = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
          let value = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
          if key = "content-length" then int_of_string_opt value else None )

  (* The next complete message body in [buf], if any. *)
  let next buf : [`Body of string | `Incomplete | `Malformed] =
    let s = Buffer.contents buf in
    match header_end s with
    | None -> `Incomplete
    | Some h -> (
        match content_length (String.sub s 0 h) with
        | None -> `Malformed
        | Some len ->
            let start = h + 4 in
            if String.length s - start < len then
              `Incomplete
            else begin
              consume buf (start + len) ;
              `Body (String.sub s start len)
            end )

  let encode json =
    let body = J.to_string json in
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body
end

(* The next complete engine reply in [buf], if any. *)
let next_reply buf : Engine.reply option =
  let len = Buffer.length buf in
  if len < Marshal.header_size then
    None
  else
    let bytes = Buffer.to_bytes buf in
    let total = Marshal.total_size bytes 0 in
    if len < total then
      None
    else begin
      consume buf total ;
      Some (Marshal.from_bytes bytes 0)
    end

(* --- State --------------------------------------------------------------- *)

type job =
  | Check of {
      uri: Uri.t;
      text: string;
    }  (** typecheck after didOpen/didSave *)
  | Tool of {
      id: Id.t;
      method_: string;
      uri: Uri.t option;
      text: string option;
      params: J.t;
    }  (** an [mlsem/*] request *)

type running = {
  job: job;
  started: float;
  progress: Lsp.Types.ProgressToken.t option;
}

type engine = {
  pid: int;
  to_engine: Unix.file_descr;
  from_engine: Unix.file_descr;
  buf: Buffer.t;
}

let engine : engine option ref = ref None
let queue : job list ref = ref []
let running : running option ref = ref None
let client_buf = Buffer.create 65536
let shutdown_received = ref false

(* Texts whose job timed out or crashed the engine, with the message
   published for them. Not tried again until the text changes. *)
let failed : (Uri.t, Digest.t * string) Hashtbl.t = Hashtbl.create 4

(* Settings from initialize. *)
let timeout = ref 10.0
let editor_pid : int option ref = ref None
let work_done_progress = ref false

let crash_times : float list ref = ref []
let max_crashes = 5
let crash_window = 180.0

(* --- Wind-down ----------------------------------------------------------- *)

let kill_engine () =
  match !engine with
  | None -> ()
  | Some e ->
      engine := None ;
      ( try Unix.kill e.pid Sys.sigkill with
      | Unix.Unix_error _ -> () ) ;
      let rec reap () =
        try ignore (Unix.waitpid [] e.pid) with
        | Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
        | Unix.Unix_error _ -> ()
      in
      reap () ;
      List.iter
        (fun fd ->
           try Unix.close fd with
           | Unix.Unix_error _ -> () )
        [e.to_engine; e.from_engine]

(* The single exit path. Exit code 0 only after a shutdown request. *)
let wind_down reason =
  let code = if !shutdown_received then 0 else 1 in
  Log.Server.info (fun m -> m "winding down (%s), exit code %d" reason code) ;
  kill_engine () ;
  exit code

(* --- Output to the client ------------------------------------------------ *)

(* Log every packet as pretty JSON. Gated by the [mlsem.server.packets] source
   (MLSEM_LOG_PACKETS=1); when the source is silent, the thunk never runs. *)
let log_packet ~dir (json : J.t) =
  Log.Packet.debug (fun m -> m "[%s] %s" dir (J.pretty_to_string json))

let write_client json =
  log_packet ~dir:"out" json ;
  let s = Frame.encode json in
  try write_all Unix.stdout s 0 (String.length s) with
  | Unix.Unix_error (e, _, _) -> wind_down ("cannot write to client: " ^ Unix.error_message e)

let send packet = write_client (Packet.yojson_of_t packet)
let send_notification n = send (Packet.Notification (Lsp.Server_notification.to_jsonrpc n))
let respond id json = send (Packet.Response (Response.ok id json))

let respond_error id code message =
  send (Packet.Response (Response.error id (Response.Error.make ~code ~message ())))

(* Ids of requests we send to the client. Nobody waits for the answers. *)
let next_request_id =
  let counter = ref 0 in
  fun () -> incr counter ; `Int !counter

let send_request (req : _ Lsp.Server_request.t) =
  send (Packet.Request (Lsp.Server_request.to_jsonrpc_request req ~id:(next_request_id ())))

let publish uri diagnostics =
  send_notification
    (Lsp.Server_notification.PublishDiagnostics
       (Lsp.Types.PublishDiagnosticsParams.create ~uri ~diagnostics ()) ) ;
  (* A save does not bump the document version, so VS Code will not re-pull
     codeLens on its own; ask it to, so the current lenses are shown. *)
  send_request Lsp.Server_request.CodeLensRefresh

(* --- Progress ------------------------------------------------------------ *)

let next_progress_token =
  let counter = ref 0 in
  fun () ->
    incr counter ;
    `String ("mlsem/typecheck/" ^ string_of_int !counter)

let progress token value =
  send_notification
    (Lsp.Server_notification.WorkDoneProgress (Lsp.Types.ProgressParams.create ~token ~value))

(* Show a spinner titled [title]; [None] if the client cannot. The create
   request is not awaited: the client handles messages in order, so the
   token exists by the time the begin notification arrives. *)
let begin_progress title =
  if not !work_done_progress then
    None
  else begin
    let token = next_progress_token () in
    send_request
      (Lsp.Server_request.WorkDoneProgressCreate
         (Lsp.Types.WorkDoneProgressCreateParams.create ~token) ) ;
    progress token (Lsp.Progress.Begin (Lsp.Types.WorkDoneProgressBegin.create ~title ())) ;
    Some token
  end

let end_progress =
  Option.iter (fun token ->
    progress token (Lsp.Progress.End (Lsp.Types.WorkDoneProgressEnd.create ())) )

(* --- Failed texts -------------------------------------------------------- *)

let error_at_start message =
  let zero = Lsp.Types.Position.create ~line:0 ~character:0 in
  Lsp.Types.Diagnostic.create ~message:(`String message)
    ~range:(Lsp.Types.Range.create ~start:zero ~end_:zero)
    ~severity:Lsp.Types.DiagnosticSeverity.Error ~source:"mlsem" ()

(* Remember that [text] of [uri] cannot be typechecked, and tell the client.
   Earlier lenses and diagnostics describe another text: drop them. *)
let mark_failed uri text message =
  Hashtbl.replace failed uri (Digest.string text, message) ;
  Doc.clear_results uri ;
  publish uri [error_at_start message]

(* [Some message] when [text] of [uri] is known to break the engine. *)
let known_failure uri text =
  match Hashtbl.find_opt failed uri with
  | Some (digest, message) when Digest.equal digest (Digest.string text) -> Some message
  | Some _ -> Hashtbl.remove failed uri ; None
  | None -> None

(* --- Engine jobs --------------------------------------------------------- *)

let spawn_engine () =
  let in_r, in_w = Unix.pipe ~cloexec:true () in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let exe = Sys.executable_name in
  let parent = string_of_int (Unix.getpid ()) in
  let pid = Unix.create_process exe [| exe; "--engine"; parent |] in_r out_w Unix.stderr in
  Unix.close in_r ;
  Unix.close out_w ;
  Log.Server.info (fun m -> m "started engine %d" pid) ;
  let e = {pid; to_engine = in_w; from_engine = out_r; buf = Buffer.create 65536} in
  engine := Some e ;
  e

let call_of = function
  | Check {uri; text} -> Engine.Check {uri; text}
  | Tool {method_; params; _} -> Engine.Tool {method_; params = J.to_string params}

(* The running job will not finish: answer or report it with [message]. *)
let fail_running message =
  Option.iter
    (fun r ->
       running := None ;
       end_progress r.progress ;
       match r.job with
       | Check {uri; text} -> mark_failed uri text message
       | Tool {id; uri; text; _} -> (
           respond_error id Response.Error.Code.RequestFailed message ;
           match (uri, text) with
           | Some uri, Some text -> mark_failed uri text message
           | _ -> () ) )
    !running

let engine_died () =
  Log.Server.err (fun m -> m "engine died") ;
  kill_engine () ;
  let now = Unix.gettimeofday () in
  crash_times := now :: List.filter (fun t -> now -. t < crash_window) !crash_times ;
  fail_running "Typechecking crashed the MLsem server" ;
  if List.length !crash_times >= max_crashes then
    wind_down (Printf.sprintf "engine crashed %d times in %.0f s" max_crashes crash_window)

(* Start the next queued job if the engine is free. *)
let rec dispatch () =
  match (!running, !queue) with
  | None, job :: rest -> (
      queue := rest ;
      let e =
        match !engine with
        | Some e -> e
        | None -> spawn_engine ()
      in
      let s = Marshal.to_string (call_of job) [] in
      match write_all e.to_engine s 0 (String.length s) with
      | exception Unix.Unix_error _ ->
          (* The engine is gone and never saw this job: retry it on a new one. *)
          queue := job :: !queue ;
          engine_died () ;
          dispatch ()
      | () ->
          let progress =
            match job with
            | Check {uri; _} ->
                begin_progress ("Typechecking " ^ Filename.basename (Uri.to_path uri))
            | Tool _ -> None
          in
          running := Some {job; started = Unix.gettimeofday (); progress} )
  | _ -> ()

let on_reply (reply : Engine.reply) =
  match (!running, reply) with
  | Some {job = Check {uri; text}; progress; _}, Checked {lenses; diagnostics} ->
      running := None ;
      end_progress progress ;
      if Doc.find uri <> None then begin
        Doc.set_result uri ~text ~lenses ~diagnostics ;
        publish uri (Doc.diagnostics uri)
      end
  | Some {job = Tool {id; _}; _}, Tool_result json ->
      running := None ;
      respond id (J.from_string json)
  | _ -> Log.Server.warn (fun m -> m "engine reply does not match the running job")

let enqueue job = queue := !queue @ [job]

(* Drop the job [id] if queued; [true] if it was. *)
let drop_queued id =
  let is_target = function
    | Tool t -> t.id = id
    | Check _ -> false
  in
  let found = List.exists is_target !queue in
  queue := List.filter (fun j -> not (is_target j)) !queue ;
  found

let cancel id =
  match !running with
  | Some {job = Tool t; _} when t.id = id ->
      kill_engine () ;
      running := None ;
      respond_error id Response.Error.Code.RequestCancelled "Request cancelled"
  | _ ->
      if drop_queued id then
        respond_error id Response.Error.Code.RequestCancelled "Request cancelled"

(* Typecheck [text] of [uri]. Older typechecks of [uri] are pointless now:
   queued ones are dropped, and a running one of another text is killed. *)
let typecheck uri text =
  match !running with
  | Some {job = Check c; _} when c.uri = uri && c.text = text ->
      (* Already on it; its result will apply. *)
      ()
  | _ -> (
      queue :=
        List.filter
          (function
            | Check c -> c.uri <> uri
            | Tool _ -> true )
          !queue ;
      ( match !running with
      | Some {job = Check c; progress; _} when c.uri = uri ->
          kill_engine () ;
          running := None ;
          end_progress progress
      | _ -> () ) ;
      match known_failure uri text with
      | Some message -> publish uri [error_at_start message]
      | None ->
          Doc.start_check uri ;
          enqueue (Check {uri; text}) )

(* --- Requests ------------------------------------------------------------ *)

let params_json (req : Request.t) : J.t =
  match req.params with
  | Some s -> (s :> J.t)
  | None -> `Null

(* Advertise incremental sync, save notifications (with text), CodeLens and
   the inline-type code action. *)
let initialize_result_json () =
  let open Lsp.Types in
  let sync =
    TextDocumentSyncOptions.create ~change:TextDocumentSyncKind.Incremental ~openClose:true
      ~save:(`SaveOptions (SaveOptions.create ~includeText:true ()))
      ()
  in
  let code_action_opts =
    CodeActionOptions.create ~codeActionKinds:[CodeActionKind.RefactorInline] ()
  in
  let capabilities =
    ServerCapabilities.create ~codeActionProvider:(`CodeActionOptions code_action_opts)
      ~codeLensProvider:(CodeLensOptions.create ~resolveProvider:false ())
      ~textDocumentSync:(`TextDocumentSyncOptions sync) ()
  in
  InitializeResult.yojson_of_t (InitializeResult.create ~capabilities ())

let read_initialize_params (params : Lsp.Types.InitializeParams.t) =
  work_done_progress :=
    Option.bind params.capabilities.window (fun (w : Lsp.Types.WindowClientCapabilities.t) ->
      w.workDoneProgress )
    |> Option.value ~default:false ;
  ( match params.initializationOptions with
  | Some (`Assoc opts) -> (
      match List.assoc_opt "typecheckTimeout" opts with
      | Some (`Int n) -> timeout := float_of_int n
      | Some (`Float f) -> timeout := f
      | _ -> () )
  | _ -> () ) ;
  editor_pid := params.processId

(* Build a [Refactor.Inline] action that prepends a [val name : type]
   signature line above the binder, indented to match the binding. A binding
   may carry several overload signatures, each emitted as its own [val] line. *)
let inline_type_action uri (range : Lsp.Types.Range.t) ~indent ~name ~tys : Lsp.Types.CodeAction.t =
  let insert_pos = Lsp.Types.Position.create ~line:range.start.line ~character:0 in
  let edit_range = Lsp.Types.Range.create ~start:insert_pos ~end_:insert_pos in
  let new_text =
    tys |> List.map (fun ty -> indent ^ "val " ^ name ^ " : " ^ ty ^ "\n") |> String.concat ""
  in
  let edit = Lsp.Types.TextEdit.create ~newText:new_text ~range:edit_range in
  let workspace_edit = Lsp.Types.WorkspaceEdit.create ~changes:[(uri, [edit])] () in
  Lsp.Types.CodeAction.create
    ~title:("Inline inferred type: " ^ String.concat " ; " tys)
    ~kind:Lsp.Types.CodeActionKind.RefactorInline ~edit:workspace_edit ()

let code_actions_for uri (req_range : Lsp.Types.Range.t) =
  Doc.lenses_in_range uri req_range
  |> List.filter_map (fun (range, signature, indent) ->
    match signature with
    | None -> None
    | Some (name, tys) -> Some (`CodeAction (inline_type_action uri range ~indent ~name ~tys)) )

(* A toolkit request becomes an engine job. Its [text] is filled in from our
   copy of the document when the client did not send it, so the engine always
   works on an explicit text. *)
let handle_tool (req : Request.t) =
  let params = params_json req in
  let uri =
    match J.Util.member "textDocument" params with
    | `Null -> None
    | td -> (
        try Some (Lsp.Types.TextDocumentIdentifier.t_of_yojson td).uri with
        | _ -> None )
  in
  let text =
    match J.Util.member "text" params with
    | `String t -> Some t
    | _ -> Option.bind uri Doc.text
  in
  let params =
    match (params, J.Util.member "text" params, text) with
    | `Assoc fields, `Null, Some t -> `Assoc (("text", `String t) :: fields)
    | _ -> params
  in
  match Option.bind uri (fun uri -> Option.bind text (known_failure uri)) with
  | Some message -> respond_error req.id Response.Error.Code.RequestFailed message
  | None -> enqueue (Tool {id = req.id; method_ = req.method_; uri; text; params})

let shutdown () =
  (* Answered here so it works even when the engine hangs. Pending work is
     abandoned: nothing will read its results. *)
  shutdown_received := true ;
  let abandon = function
    | Tool {id; _} -> respond_error id Response.Error.Code.RequestFailed "Server is shutting down"
    | Check _ -> ()
  in
  Option.iter (fun r -> end_progress r.progress ; abandon r.job) !running ;
  running := None ;
  List.iter abandon !queue ;
  queue := [] ;
  kill_engine ()

let handle_request (req : Request.t) =
  if !shutdown_received then
    respond_error req.id Response.Error.Code.InvalidRequest "Server is shutting down"
  else if Engine.is_tool req.method_ then
    handle_tool req
  else
    match
      Lsp.Client_request.of_jsonrpc req
    with
    | Error err ->
        Log.Server.warn (fun m -> m "request decode error: %s" err) ;
        respond_error req.id Response.Error.Code.MethodNotFound "Method not found"
    | Ok (Lsp.Client_request.E typed_req) -> (
        match typed_req with
        | Lsp.Client_request.Initialize params ->
            read_initialize_params params ;
            respond req.id (initialize_result_json ())
        | Lsp.Client_request.Shutdown -> shutdown () ; respond req.id `Null
        | Lsp.Client_request.CodeAction params ->
            let actions = code_actions_for params.textDocument.uri params.range in
            let result = if actions = [] then None else Some actions in
            respond req.id (Lsp.Client_request.yojson_of_result typed_req result)
        | Lsp.Client_request.TextDocumentCodeLens params ->
            (* Answered from the copy, also while a typecheck runs: the lenses
               are the previous ones shifted through edits, and a refresh
               follows the new result. *)
            let lenses = Doc.code_lenses params.textDocument.uri in
            respond req.id (Lsp.Client_request.yojson_of_result typed_req lenses)
        | _ -> respond_error req.id Response.Error.Code.MethodNotFound "Method not found" )

(* --- Notifications ------------------------------------------------------- *)

let handle_notification (notif : Notification.t) =
  match Lsp.Client_notification.of_jsonrpc notif with
  | Ok Lsp.Client_notification.Exit -> wind_down "exit notification"
  | _ when !shutdown_received -> ()
  | Error err -> Log.Server.warn (fun m -> m "notification decode error: %s" err)
  | Ok (Lsp.Client_notification.CancelRequest id) -> cancel id
  | Ok (Lsp.Client_notification.TextDocumentDidOpen params) ->
      let uri = params.textDocument.uri in
      Doc.open_ uri params.textDocument.text ;
      typecheck uri params.textDocument.text
  | Ok (Lsp.Client_notification.TextDocumentDidChange params) ->
      (* Events are ordered; each one's range refers to the buffer after the
         previous ones. *)
      List.iter (Doc.apply_change params.textDocument.uri) params.contentChanges
  | Ok (Lsp.Client_notification.DidSaveTextDocument params) -> (
      let uri = params.textDocument.uri in
      match Option.fold ~none:(Doc.text uri) ~some:Option.some params.text with
      | None -> Log.Server.warn (fun m -> m "didSave of an unknown document without text")
      | Some text ->
          (* VS Code sends didSave on every Ctrl+S, also for unchanged
             buffers: then only publish again. *)
          if Doc.up_to_date uri text then publish uri (Doc.diagnostics uri) else typecheck uri text
      )
  | Ok (Lsp.Client_notification.TextDocumentDidClose params) ->
      let uri = params.textDocument.uri in
      Doc.close uri ;
      Hashtbl.remove failed uri ;
      queue :=
        List.filter
          (function
            | Check c -> c.uri <> uri
            | Tool _ -> true )
          !queue
  | Ok _ -> ()

(* [Jsonrpc] only accepts an object or array as [params]; some clients send
   [null] for "no params". *)
let drop_null_params = function
  | `Assoc fields -> `Assoc (List.filter (fun (k, v) -> not (k = "params" && v = `Null)) fields)
  | json -> json

let handle_packet (json : J.t) =
  log_packet ~dir:"in" json ;
  let json =
    match json with
    | `List l -> `List (List.map drop_null_params l)
    | json -> drop_null_params json
  in
  match Packet.t_of_yojson json with
  | exception e -> Log.Server.warn (fun m -> m "invalid packet: %s" (Printexc.to_string e))
  | Packet.Request req -> handle_request req
  | Packet.Notification notif -> handle_notification notif
  | Packet.Batch_call calls ->
      List.iter
        (function
          | `Request r -> handle_request r
          | `Notification n -> handle_notification n )
        calls
  | Packet.Response _
  | Packet.Batch_response _ ->
      (* Answers to our requests; nobody waits for them. *)
      ()

(* --- Main loop ----------------------------------------------------------- *)

let pending_signal : string option ref = ref None

let install_signal_handlers () =
  (* A write to a closed pipe then fails with EPIPE, which we handle, instead
     of killing us silently. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  List.iter
    (fun (signal, name) ->
       Sys.set_signal signal (Sys.Signal_handle (fun _ -> pending_signal := Some name)) )
    [(Sys.sigterm, "SIGTERM"); (Sys.sigint, "SIGINT"); (Sys.sighup, "SIGHUP")]

let rec drain_client () =
  match Frame.next client_buf with
  | `Incomplete -> ()
  | `Malformed -> wind_down "malformed message header from client"
  | `Body body ->
      ( match J.from_string body with
      | json -> handle_packet json
      | exception Yojson.Json_error e -> Log.Server.warn (fun m -> m "invalid JSON: %s" e) ) ;
      drain_client ()

let rec drain_engine e =
  match next_reply e.buf with
  | None -> ()
  | Some reply -> on_reply reply ; drain_engine e

let tick = 1.0

let deadline () =
  match !running with
  | Some r when !timeout > 0.0 -> Some (r.started +. !timeout)
  | _ -> None

let check_timeout () =
  match (!running, deadline ()) with
  | Some r, Some d when Unix.gettimeofday () >= d ->
      kill_engine () ;
      let limit = Printf.sprintf "%g s" !timeout in
      fail_running
        ( match r.job with
        | Check _ -> "Typechecking timed out after " ^ limit
        | Tool _ -> "Request timed out after " ^ limit )
  | _ -> ()

let last_editor_check = ref 0.0

let check_editor () =
  let now = Unix.gettimeofday () in
  if now -. !last_editor_check >= tick then begin
    last_editor_check := now ;
    match !editor_pid with
    | Some pid -> (
        try Unix.kill pid 0 with
        | Unix.Unix_error (Unix.ESRCH, _, _) -> wind_down "editor process is gone"
        | Unix.Unix_error _ -> () )
    | None -> ()
  end

let rec loop () =
  Option.iter (fun name -> wind_down ("received " ^ name)) !pending_signal ;
  let engine_fd = Option.map (fun e -> e.from_engine) !engine in
  let fds = Unix.stdin :: Option.to_list engine_fd in
  let wait =
    match deadline () with
    | Some d -> Float.max 0.0 (Float.min tick (d -. Unix.gettimeofday ()))
    | None -> tick
  in
  match Unix.select fds [] [] wait with
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  | ready, _, _ ->
      (* Engine output first, so a job that finished just in time is not
         counted as timed out. *)
      ( match !engine with
      | Some e when List.mem e.from_engine ready ->
          if read_into e.from_engine e.buf then drain_engine e else engine_died ()
      | _ -> () ) ;
      if List.mem Unix.stdin ready then
        if read_into Unix.stdin client_buf then drain_client () else wind_down "client closed stdin" ;
      check_timeout () ;
      check_editor () ;
      dispatch () ;
      loop ()

let run () =
  Log.setup () ;
  install_signal_handlers () ;
  Log.Server.info (fun m -> m "server started") ;
  loop ()
