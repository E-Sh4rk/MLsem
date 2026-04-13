open Mlsem_common
open Mlsem_app
open Mlsem_app.Main.NoExt
open Mlsem_utils
open Mlsem_types

(* Parameters *)
let record = ref false
let notime = ref false
let input_files = ref []

(* Printing results *)
let severity_to_str s =
    match s with
    | Mlsem_system.Analyzer.Error -> "Error"
    | Warning -> "Warning"
    | Notice -> "Notice"
    | Message -> "Message"

let treat_res (acc, res) =
    match res with
    | TSuccess (lst, msg, time) ->
        lst |> List.iter (fun (v,t) ->
            Format.printf "@{<blue;bold>%s@}: %s"
                (Variable.get_name v |> Option.get) t ;
            if !notime |> not then
                Format.printf " @{<italic;yellow>(checked in %.00fms)@}" time ;
            Format.printf "\n%!"
        ) ;
        msg |> List.iter (fun (s,pos,title,descr) ->
            Format.printf "@{<italic;bold;cyan>[%s]@} @{<italic;cyan>%s@} @{<italic;cyan>%s@}\n%!"
                (severity_to_str s) (Position.string_of_pos pos) title ;
            descr |> Option.iter (Format.printf "@{<italic;cyan>%s@}\n%!") ;
        ) ;
        acc, true
    | TFailure (Some v, pos, msg, descr, time) ->
        Format.printf "@{<blue;bold>%s@}: @{<red>%s@}"
            (Variable.get_name v |> Option.get) msg ;
        if !notime |> not then
            Format.printf " @{<italic;purple>(failed in %.00fms)@}" time ;
        Format.printf "\n%!" ;
        Format.printf "@{<italic;cyan>%s@}\n%!" (Position.string_of_pos pos) ;
        descr |> Option.iter (Format.printf "@{<italic;cyan>%s@}\n%!") ;
        acc, false
    | TFailure (None, _, msg, _, _) ->
        Format.printf "@{<red>%s@}\n%!" msg ;
        acc, false
    | TDone -> acc, true

(* Command line *)
let usage_msg = "mlsem [-record] [-notime] <file1> [<file2>] ..."
let anon_fun filename =
    input_files := filename::!input_files

let speclist =
    [("-record", Arg.Set record, "Record tallying instances into a file") ;
     ("-notime", Arg.Set notime, "Do not print times")]

let () =
    Arg.parse speclist anon_fun usage_msg ;
    (* Printexc.record_backtrace true; *)
    if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter ;
    if !record then Recording.start_recording () ;
    try
        let time = Unix.gettimeofday () in
        List.rev !input_files |> List.iter (fun fn ->
            Format.printf "@.@{<bold>===== Processing %s =====@}@." fn ;
            Recording.clear () ; Config.restore_all () ;
            match parse (`File fn) with
            | PSuccess program ->
                let time0 = Unix.gettimeofday () in
                let envs = initial_envs in
                let envs, ok = treat_all_sigs envs program |> treat_res in
                if ok then
                    List.fold_left (fun acc e ->
                        treat_def acc e |> treat_res |> fst
                    ) envs program |> ignore ;
                let time1 = Unix.gettimeofday () in
                if !notime |> not then
                    Format.printf "@{<bold;green>Total time: %.02fs@}@." (time1 -. time0) ;
                if !record then Recording.save_to_file fn (Recording.tally_calls ())
            | PFailure (pos, msg) ->
                Format.printf "@{<bold;red>%s: %s@}@." (Position.string_of_pos pos) msg
        ) ;
        let time = (Unix.gettimeofday ()) -. time in
        if !notime |> not then
            Format.printf "@.@{<bold>Cumulated total time: %.02fs@}@." time
    with e ->
        let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
        Format.printf "@.Uncaught exception: %s\n%s@." msg stack
