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
    List.iter (output_string oc) chunks;
    flush oc
end

(* LSP transport handles Content-Length framing and packet encoding/decoding. *)
module Transport = Lsp.Io.Make (Sync_io) (Chan)

(* Log every packet as pretty JSON. Gated by the [mlsem.server.packets] source
   (MLSEM_LOG_PACKETS=1); when the source is silent, the thunk never runs and
   the JSON is never built. *)
let log_packet ~dir (packet : Packet.t) =
  Log.Packet.debug (fun m ->
      m "[%s] %s" dir
        (packet |> Packet.yojson_of_t |> Yojson.Safe.pretty_to_string))

let send packet =
  log_packet ~dir:"out" packet;
  Transport.write stdout packet

(* Minimal initialize reply so editors can complete startup handshake. *)
let initialize_result_json () =
  let open Lsp.Types in
  let capabilities =
    ServerCapabilities.create
      ~textDocumentSync:(`TextDocumentSyncKind TextDocumentSyncKind.Full)
      ()
  in
  let result = InitializeResult.create ~capabilities () in
  InitializeResult.yojson_of_t result

let method_not_found id =
  let err =
    Response.Error.make
      ~code:Response.Error.Code.MethodNotFound
      ~message:"Method not found"
      ()
  in
  Packet.Response (Response.error id err)

(* Handle client requests via typed LSP decoding. *)
let handle_request ~running (req : Jsonrpc.Request.t) =
  match Lsp.Client_request.of_jsonrpc req with
  | Error err ->
      Log.Server.warn (fun m -> m "request decode error: %s" err) ;
      send (method_not_found req.id) ;
      running
  | Ok (Lsp.Client_request.E typed_req) ->
      (match typed_req with
      | Lsp.Client_request.Initialize _ ->
          send (Packet.Response (Response.ok req.id (initialize_result_json ())));
          running
      | Lsp.Client_request.Shutdown ->
          send (Packet.Response (Response.ok req.id `Null));
          false
      | Lsp.Client_request.UnknownRequest _ ->
          send (method_not_found req.id);
          running
      | _ ->
          send (method_not_found req.id);
          running)

(* Handle client notifications via typed LSP decoding. *)
let handle_notification ~running (notif : Jsonrpc.Notification.t) =
  match Lsp.Client_notification.of_jsonrpc notif with
  | Error err ->
      Log.Server.warn (fun m -> m "notification decode error: %s" err) ;
      running
  | Ok Lsp.Client_notification.Exit ->
      if running then exit 1 else exit 0
  | Ok _ -> running

(* Batch calls can mix requests and notifications. *)
let rec handle_batch ~running = function
  | [] -> running
  | (`Notification notif) :: rest ->
      let running = handle_notification ~running notif in
      handle_batch ~running rest
  | (`Request req) :: rest ->
      let running = handle_request ~running req in
      handle_batch ~running rest

(* Main server loop: read packet, log it, dispatch, repeat. *)
let rec loop ~running =
  match Transport.read stdin with
  | None -> ()
  | Some packet ->
      log_packet ~dir:"in" packet;
      let running =
        match packet with
        | Packet.Request req -> handle_request ~running req
        | Packet.Notification notif -> handle_notification ~running notif
        | Packet.Batch_call calls -> handle_batch ~running calls
        | Packet.Response _ -> running
        | Packet.Batch_response _ -> running
      in
      loop ~running

let run () =
  Log.setup () ;
  loop ~running:true
