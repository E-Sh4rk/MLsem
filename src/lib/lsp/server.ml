open Jsonrpc

(* Minimal identity monad so we can instantiate Lsp.Io.Make synchronously. *)
module Sync_io = struct
  type 'a t = 'a

  let return x = x
  let raise e = Stdlib.raise e

  module O = struct
    let ( let+ ) x f = f x
    let ( let* ) x f = f x
  end
end

(* Channel adapter from stdio to the operations required by Lsp.Io.Make. *)
module Chan = struct
  type input = in_channel
  type output = out_channel

  let read_line ic =
    try Some (input_line ic) with
    | End_of_file -> None

  let read_exactly ic len =
    try Some (really_input_string ic len) with
    | End_of_file -> None

  let write oc chunks =
    List.iter (output_string oc) chunks ;
    flush oc
end

(* LSP transport handles Content-Length framing and packet encoding/decoding. *)
module Transport = Lsp.Io.Make (Sync_io) (Chan)

(* Raised to unwind the server loop with a specific process exit code.
   Lets [run] own cleanup instead of scattering [exit] calls through the loop. *)
exception Exit_requested of int

(* Log every packet as pretty JSON. Gated by the [mlsem.server.packets] source
   (MLSEM_LOG_PACKETS=1); when the source is silent, the thunk never runs and
   the JSON is never built. *)
let log_packet ~dir (packet : Packet.t) =
  Log.Packet.debug (fun m ->
    m "[%s] %s" dir (packet |> Packet.yojson_of_t |> Yojson.Safe.pretty_to_string) )

let send packet = log_packet ~dir:"out" packet ; Transport.write stdout packet

let send_notification n = send (Packet.Notification (Lsp.Server_notification.to_jsonrpc n))

(* Server-initiated request ids. They share no namespace with client request
   ids, so a private counter suffices. We never wait on the response — the
   main loop drops inbound responses — so these are fire-and-forget. *)
let next_request_id =
  let counter = ref 0 in
  fun () -> incr counter ; `Int !counter

let send_request (req : _ Lsp.Server_request.t) =
  let jsonrpc = Lsp.Server_request.to_jsonrpc_request req ~id:(next_request_id ()) in
  send (Packet.Request jsonrpc)

(* Raw params as Yojson for the custom [mlsem/*] handlers. [Jsonrpc.Structured.t]
   ([`Assoc]/[`List]) is a structural subtype of [Yojson.Safe.t], so the
   coercion is total and allocation-free. *)
let params_json (req : Jsonrpc.Request.t) : Yojson.Safe.t =
  match req.params with
  | Some s -> (s :> Yojson.Safe.t)
  | None -> `Null

(* Advertise full-sync text, save notifications (with text), and CodeLens. *)
let initialize_result_json () =
  let open Lsp.Types in
  let sync =
    (* Incremental sync: didChange events carry [range]+[text] for each
       edit, which lets [Store.apply_change] shift cached lens offsets
       through the edit instead of dropping them. *)
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
  let result = InitializeResult.create ~capabilities () in
  InitializeResult.yojson_of_t result

let method_not_found id =
  let err =
    Response.Error.make ~code:Response.Error.Code.MethodNotFound ~message:"Method not found" ()
  in
  Packet.Response (Response.error id err)

(* Run the typechecker against the current document text, cache the result,
   and emit diagnostics. Called on didOpen and didSave.

   Skips only the (expensive) re-typecheck when [text] matches what was last
   typechecked — VS Code fires didSave on every Ctrl+S, including saves of
   unchanged buffers. The diagnostic publish below is *not* skipped: a custom
   panel request may have typechecked this exact text via [Store.sync_text]
   (advancing the digest) without ever publishing, so the client does not
   necessarily already have the diagnostics. *)
let typecheck_and_publish uri text =
  if Store.matches_typechecked_digest uri text then
    Log.Server.debug (fun m -> m "skipping re-typecheck: content unchanged")
  else begin
    let result = Typecheck.run text in
    Store.set_result uri ~text ~result
  end ;
  let params =
    Lsp.Types.PublishDiagnosticsParams.create ~uri ~diagnostics:(Store.diagnostics uri) ()
  in
  send_notification (Lsp.Server_notification.PublishDiagnostics params) ;
  (* A save does not bump the document version, so VS Code will not re-pull
     codeLens on its own after didSave — it would keep showing the lenses from
     before the edit until some other event invalidates them (e.g. switching
     files). Ask the client to refresh so the current lenses are pulled. *)
  send_request Lsp.Server_request.CodeLensRefresh

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
  Store.lenses_in_range uri req_range
  |> List.filter_map (fun (range, signature, indent) ->
    match signature with
    | None -> None
    | Some (name, tys) -> Some (`CodeAction (inline_type_action uri range ~indent ~name ~tys)) )

(* Handle client requests via typed LSP decoding. Custom [mlsem/*] methods are
   intercepted first: the typed decoder would map them to [UnknownRequest], and
   they carry plain-JSON params the [Handlers] module decodes itself. *)
let handle_request ~shutdown_received (req : Jsonrpc.Request.t) =
  match req.method_ with
  | "mlsem/overloads" ->
      send (Packet.Response (Response.ok req.id (Handlers.overloads (params_json req)))) ;
      shutdown_received
  | "mlsem/mergePreview" ->
      send (Packet.Response (Response.ok req.id (Handlers.merge_preview (params_json req)))) ;
      shutdown_received
  | "mlsem/applyMerge" ->
      send (Packet.Response (Response.ok req.id (Handlers.apply_merge (params_json req)))) ;
      shutdown_received
  | "mlsem/instantiatePreview" ->
      send (Packet.Response (Response.ok req.id (Handlers.instantiate_preview (params_json req)))) ;
      shutdown_received
  | "mlsem/applyInstantiate" ->
      send (Packet.Response (Response.ok req.id (Handlers.apply_instantiate (params_json req)))) ;
      shutdown_received
  | _ -> (
      match Lsp.Client_request.of_jsonrpc req with
      | Error err ->
          Log.Server.warn (fun m -> m "request decode error: %s" err) ;
          send (method_not_found req.id) ;
          shutdown_received
      | Ok (Lsp.Client_request.E typed_req) -> (
          match typed_req with
          | Lsp.Client_request.Initialize _ ->
              send (Packet.Response (Response.ok req.id (initialize_result_json ()))) ;
              shutdown_received
          | Lsp.Client_request.Shutdown ->
              send (Packet.Response (Response.ok req.id `Null)) ;
              true
          | Lsp.Client_request.CodeAction params ->
              let uri = params.textDocument.uri in
              let actions = code_actions_for uri params.range in
              let result = if actions = [] then None else Some actions in
              let json = Lsp.Client_request.yojson_of_result typed_req result in
              send (Packet.Response (Response.ok req.id json)) ;
              shutdown_received
          | Lsp.Client_request.TextDocumentCodeLens params ->
              (* Lens offsets are kept in sync with the live buffer via
             [Store.apply_change] on each didChange, so the projection to
             LSP Range is against the current line layout. *)
              let uri = params.textDocument.uri in
              let lenses = Store.code_lenses uri in
              let json = Lsp.Client_request.yojson_of_result typed_req lenses in
              send (Packet.Response (Response.ok req.id json)) ;
              shutdown_received
          | Lsp.Client_request.UnknownRequest _ ->
              send (method_not_found req.id) ;
              shutdown_received
          | _ ->
              send (method_not_found req.id) ;
              shutdown_received ) )

(* Handle client notifications via typed LSP decoding. *)
let handle_notification ~shutdown_received (notif : Jsonrpc.Notification.t) =
  match Lsp.Client_notification.of_jsonrpc notif with
  | Error err ->
      Log.Server.warn (fun m -> m "notification decode error: %s" err) ;
      shutdown_received
  | Ok Lsp.Client_notification.Exit ->
      if shutdown_received then
        raise (Exit_requested 0)
      else (
        Log.Server.warn (fun m -> m "exit without prior Shutdown") ;
        raise (Exit_requested 1) )
  | Ok (Lsp.Client_notification.CancelRequest _id) ->
      Log.Server.info (fun m -> m "$/cancelRequest is not supported, dropping") ;
      shutdown_received
  | Ok (Lsp.Client_notification.DidSaveTextDocument params) ->
      let uri = params.textDocument.uri in
      ( match params.text with
      | Some text -> typecheck_and_publish uri text
      | None ->
          Log.Server.warn (fun m -> m "didSave without text despite includeText:true; skipping") ) ;
      shutdown_received
  | Ok (Lsp.Client_notification.TextDocumentDidChange params) ->
      (* Apply each edit to the cached buffer and shift lens offsets
         through it. Events are ordered; each subsequent event's range
         refers to the buffer state after prior events have been applied. *)
      let uri = params.textDocument.uri in
      List.iter (Store.apply_change uri) params.contentChanges ;
      shutdown_received
  | Ok (Lsp.Client_notification.TextDocumentDidClose params) ->
      Store.remove params.textDocument.uri ;
      shutdown_received
  | Ok (Lsp.Client_notification.TextDocumentDidOpen params) ->
      let uri = params.textDocument.uri in
      typecheck_and_publish uri params.textDocument.text ;
      shutdown_received
  | Ok _ -> shutdown_received

(* Batch calls can mix requests and notifications. *)
let rec handle_batch ~shutdown_received = function
  | [] -> shutdown_received
  | `Notification notif :: rest ->
      let shutdown_received = handle_notification ~shutdown_received notif in
      handle_batch ~shutdown_received rest
  | `Request req :: rest ->
      let shutdown_received = handle_request ~shutdown_received req in
      handle_batch ~shutdown_received rest

(* Main server loop: read packet, log it, dispatch, repeat. *)
let rec loop ~shutdown_received =
  match Transport.read stdin with
  | None ->
      if shutdown_received then
        raise (Exit_requested 0)
      else (
        Log.Server.err (fun m -> m "stdin closed without prior Shutdown") ;
        raise (Exit_requested 1) )
  | Some packet ->
      log_packet ~dir:"in" packet ;
      let shutdown_received =
        match packet with
        | Packet.Request req -> handle_request ~shutdown_received req
        | Packet.Notification notif -> handle_notification ~shutdown_received notif
        | Packet.Batch_call calls -> handle_batch ~shutdown_received calls
        | Packet.Response _ -> shutdown_received
        | Packet.Batch_response _ -> shutdown_received
      in
      loop ~shutdown_received

let run () =
  Log.setup () ;
  Log.Server.info (fun m -> m "server started") ;
  try loop ~shutdown_received:false with
  | Exit_requested code -> exit code
