(* Logging sources for the LSP server.

   - [src]     — general server messages (handshake, decode errors, lifecycle).
   - [src_pkt] — raw JSON-RPC packets crossing the wire; very chatty, off by default.

   Levels (error/warning/info/debug) are the severity knob; sources let us turn
   packet tracing on independently of the general log level. *)

let src = Logs.Src.create "mlsem.server" ~doc:"LSP server"
let src_pkt = Logs.Src.create "mlsem.server.packets" ~doc:"LSP wire packets"

module Server = (val Logs.src_log src : Logs.LOG)
module Packet = (val Logs.src_log src_pkt : Logs.LOG)

let level_of_string = function
  | "error" -> Some Logs.Error
  | "warn" -> Some Logs.Warning
  | "info" -> Some Logs.Info
  | "debug" -> Some Logs.Debug
  | _ -> None

(* Configure the global reporter and per-source levels from the environment:
     MLSEM_LOG         = error | warn | info | debug   (default: info)
     MLSEM_LOG_PACKETS = 1                             (default: off) *)
let setup () =
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.err_formatter ()) ;
  let level =
    Option.bind (Sys.getenv_opt "MLSEM_LOG") level_of_string
    |> Option.value ~default:Logs.Info
  in
  Logs.set_level ~all:true (Some level) ;
  Logs.Src.set_level src_pkt
    (if Sys.getenv_opt "MLSEM_LOG_PACKETS" = Some "1" then Some Logs.Debug
     else None)
