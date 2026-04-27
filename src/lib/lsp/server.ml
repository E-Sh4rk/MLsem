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

(* Advertise full-sync text, save notifications (with text), and CodeLens. *)
let initialize_result_json () =
  let open Lsp.Types in
  let sync =
    (* TODO *)
    TextDocumentSyncOptions.create ~change:TextDocumentSyncKind.Full ~openClose:true
      ~save:(`SaveOptions (SaveOptions.create ~includeText:true ()))
      ()
  in
  let capabilities =
    ServerCapabilities.create
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

   Skips the run (and the diagnostic re-publish, which the client already
   has) when [text] matches what was last typechecked — VS Code fires
   didSave on every Ctrl+S, including saves of unchanged buffers. *)
let typecheck_and_publish uri text =
  if Store.is_cached uri text then
    Log.Server.debug (fun m -> m "skipping typecheck: content unchanged")
  else
    let result = Typecheck.run text in
    Store.set uri ~text ~result ;
    let params =
      Lsp.Types.PublishDiagnosticsParams.create ~uri ~diagnostics:result.diagnostics ()
    in
    send_notification (Lsp.Server_notification.PublishDiagnostics params)

(* Handle client requests via typed LSP decoding. *)
let handle_request ~shutdown_received (req : Jsonrpc.Request.t) =
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
      | Lsp.Client_request.TextDocumentCodeLens params ->
          let uri = params.textDocument.uri in
          let lenses = (Store.result uri).code_lenses in
          let json = Lsp.Client_request.yojson_of_result typed_req lenses in
          send (Packet.Response (Response.ok req.id json)) ;
          shutdown_received
      | Lsp.Client_request.UnknownRequest _ ->
          send (method_not_found req.id) ;
          shutdown_received
      | _ ->
          send (method_not_found req.id) ;
          shutdown_received )

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
  try loop ~shutdown_received:false with
  | Exit_requested code -> exit code
