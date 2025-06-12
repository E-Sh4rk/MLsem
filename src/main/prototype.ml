open Main

let treat_res (acc, res) =
    match res with
    | TSuccess (lst, time) ->
        lst |> List.iter (fun (v,t) ->
            Format.printf "@{<blue;bold>%s@}: %!"
                (Parsing.Variable.Variable.get_name v |> Option.get) ;
            Format.printf "%a @{<italic;yellow>(checked in %.00fms)@}\n%!"
                Types.TyScheme.pp_short t time ;
        ) ;
        acc, true
    | TFailure (Some v, _, msg, time) ->
        Format.printf "@{<blue;bold>%s@}: %!"
            (Parsing.Variable.Variable.get_name v |> Option.get) ;
        Format.printf "@{<red>%s@} @{<italic;purple>(failed in %.00fms)@}\n%!" msg time ;
        acc, false
    | TFailure (None, _, msg, _) ->
        Format.printf "@{<red>%s@}\n%!" msg ;
        acc, false
    | TDone -> acc, true

let () =
    (* Printexc.record_backtrace true; *)
    if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter ;
    try
        let fn = ref "test.ml" in
        if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;
        match parse (`File !fn) with
        | PSuccess program ->
            let time0 = Unix.gettimeofday () in
            let envs = (initial_tenv, initial_varm, initial_senv, initial_env) in
            let envs, ok = treat_all_sigs envs program |> treat_res in
            if ok then
                List.fold_left (fun acc e ->
                    treat_def acc e |> treat_res |> fst
                ) envs program |> ignore ;
            let time1 = Unix.gettimeofday () in
            Format.printf "@.@{<bold;green>Total time: %.02fs@}@." (time1 -. time0)
        | PFailure (pos, msg) ->
            Format.printf "@{<bold;red>%s: %s@}@." (Position.string_of_pos pos) msg
    with e ->
        let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
        Format.printf "@.Uncaught exception: %s\n%s@." msg stack
