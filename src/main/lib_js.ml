open Main
open Js_of_ocaml
open Yojson.Basic
open Variable

module Html = Dom_html

let json_of_pos pos =
  let open Position in
  if pos = Position.dummy
  then
    `Assoc [("startLine", `Int (-1)) ; ("startCol", `Int (-1)) ;
    ("endLine", `Int (-1)) ; ("endCol", `Int (-1)) ;
    ("startOffset", `Int (-1)) ; ("endOffset", `Int (-1))]
  else
    let startp = start_of_position pos in
    let endp = end_of_position pos in
    `Assoc [("startLine", `Int (line startp)) ; ("startCol", `Int (column startp)) ;
    ("endLine", `Int (line endp)) ; ("endCol", `Int (column endp)) ;
    ("startOffset", `Int (offset startp)) ; ("endOffset", `Int (offset endp))]

let add_res res res' =
  match res' with
  | TDone -> res
  | TFailure (Some v, pos, msg, descr, time) ->
    let name = Variable.get_name v |> Option.get in
    let def_pos = Variable.get_location v in
    let descr = match descr with None -> [] | Some d -> [("descr", `String d)] in
    let untyp =
      `Assoc ([("name", `String name) ; ("def_pos", json_of_pos def_pos) ; ("time", `Float time) ;
      ("typeable", `Bool false) ; ("message", `String msg) ; ("pos", json_of_pos pos)]@descr)
    in
    untyp::res
  | TFailure (None, pos, msg, descr, time) ->
    let descr = match descr with None -> [] | Some d -> [("descr", `String d)] in
    let untyp =
      `Assoc ([("time", `Float time) ; ("def_pos", json_of_pos pos) ;
      ("typeable", `Bool false) ; ("message", `String msg) ; ("pos", json_of_pos pos)]@descr)
    in
    untyp::res
  | TSuccess (lst,time) ->
    let res = ref res in
    lst |> List.iter (fun (v,t)->
      let name = Variable.get_name v |> Option.get in
      let def_pos = Variable.get_location v in
      let typ = Format.asprintf "%a" Types.TyScheme.pp_short t in
      let typ =
        `Assoc [("name", `String name) ; ("def_pos", json_of_pos def_pos) ;
        ("typeable", `Bool true) ; ("type", `String typ) ; ("time", `Float time)]
      in
      res := typ::!res
    ) ;
  !res

let ok_answer res =
  `Assoc [("exit_code", `Int 0); ("results", `List (List.rev res))]
let notify_res callback res =
  if Js.Opt.test callback then (
    let intermediate_answer = ok_answer res |> to_string |> Js.string in
    Js.Unsafe.fun_call callback [| intermediate_answer |> Js.Unsafe.inject |] |> ignore
  )

let typecheck code callback =
  let res =
    try (
      match parse (`String (Js.to_string code)) with
      | PSuccess program ->
        let envs = (initial_tenv, initial_varm, initial_senv, initial_env) in
        let envs,res = treat_all_sigs envs program in
        let ok = match res with | TFailure _ -> false | _ -> true in
        let res = add_res [] res in
        notify_res callback res ;
        let (_, res) =
          if ok then
            List.fold_left (fun (env, res) e ->
              let env, res' = treat_def env e in
              let res = add_res res res' in
              notify_res callback res ;
              (env,res)
            ) (envs, res) program
          else (envs, res)
        in
        ok_answer res
      | PFailure (pos, msg) ->
        `Assoc [("exit_code", `Int (-2)); ("message", `String msg); ("pos", json_of_pos pos)]
    ) with e ->
      `Assoc [("exit_code", `Int (-1)); ("message", `String ("internal error: "^(Printexc.to_string e))); ("pos", `List [])]
  in
  Js.string (to_string res)

let _ =
  Js.export "checker"
    (object%js
       method typecheck code callback = typecheck code callback
     end)
