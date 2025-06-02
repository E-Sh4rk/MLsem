open Main
open Js_of_ocaml
open Yojson.Basic

module Html = Dom_html

let json_of_pos pos =
  let open Position in
  let startp = start_of_position pos in
  let endp = end_of_position pos in
  `Assoc [("startLine", `Int (line startp)) ; ("startCol", `Int (column startp)) ;
  ("endLine", `Int (line endp)) ; ("endCol", `Int (column endp)) ;
  ("startOffset", `Int (offset startp)) ; ("endOffset", `Int (offset endp))]

let json_of_pos_list pos =
  `List (List.map json_of_pos pos)

let typecheck code callback =
  let res =
    try (
      match parse (`String (Js.to_string code)) with
      | PSuccess program ->
        let ok_answer res =
          `Assoc [("exit_code", `Int 0); ("results", `List (List.rev res))]
        in
        let (_, res) =
          List.fold_left (fun (env, res) e ->
            let env, res' = treat env e in
            let res = match res' with
            | TDone -> res
            | TFailure (Some v, pos, msg, time) ->
              let name = Parsing.Variable.Variable.get_name v |> Option.get in
              let def_pos = Parsing.Variable.Variable.get_locations v |> List.hd in
              let untyp =
                `Assoc [("name", `String name) ; ("def_pos", json_of_pos def_pos) ; ("time", `Float time) ;
                ("typeable", `Bool false) ; ("message", `String msg) ; ("pos", json_of_pos_list pos)]
              in
              untyp::res
            | TFailure (None, pos, msg, time) ->
              let untyp =
                `Assoc [("time", `Float time) ;
                ("typeable", `Bool false) ; ("message", `String msg) ; ("pos", json_of_pos_list pos)]
              in
              untyp::res
            | TSuccess (v,t,time) ->
              let name = Parsing.Variable.Variable.get_name v |> Option.get in
              let def_pos = Parsing.Variable.Variable.get_locations v |> List.hd in
              let typ = Format.asprintf "%a" Types.TyScheme.pp_short t in
              let typ =
                `Assoc [("name", `String name) ; ("def_pos", json_of_pos def_pos) ;
                ("typeable", `Bool true) ; ("type", `String typ) ; ("time", `Float time)]
              in
              typ::res
            in
            if Js.Opt.test callback then (
              let intermediate_answer = ok_answer res |> to_string |> Js.string in
              Js.Unsafe.fun_call callback [| intermediate_answer |> Js.Unsafe.inject |] |> ignore
            ) ;
            (env,res)
          ) ((initial_tenv, initial_varm, initial_senv, initial_env), []) program
        in
        ok_answer res
      | PFailure (pos, msg) ->
        `Assoc [("exit_code", `Int (-2)); ("message", `String msg); ("pos", json_of_pos_list [pos])]
    ) with e ->
      `Assoc [("exit_code", `Int (-1)); ("message", `String ("internal error: "^(Printexc.to_string e))); ("pos", `List [])]
  in
  Js.string (to_string res)

let _ =
  Js.export "checker"
    (object%js
       method typecheck code callback = typecheck code callback
     end)
