open Tvar
open Base
open Yojson.Safe
open Recording_internal

let start_recording () = recording := true
let stop_recording () = recording := false

(* === Recording of tallying calls === *)
type tally_call = Recording_internal.tally_call
let clear () = tally_calls := []
let tally_calls () = List.rev !tally_calls

let save_to_file file tallys =
  let ty_to_string ty = `String (Format.asprintf "%a" Ty.pp_raw ty) in
  let instances = tallys |> List.map (fun (r:tally_call) ->
      let s = TVOp.shorten_names (TVarSet.construct r.vars) in
      let ty_to_string ty = Subst.apply s ty |> ty_to_string in
      let to_str = List.map (fun v -> TVar.typ v |> ty_to_string) in
      let vars, mono = to_str r.vars, to_str r.mono in
      let cs = r.constraints |> List.map (fun (s,t) ->
          `List  [ty_to_string s ; ty_to_string t]
          )
      in
      `Assoc [ ("vars", `List vars) ; ("mono", `List mono) ; ("constr", `List cs) ]
  ) in
  let file = (Filename.remove_extension file)^".json" in
  let oc = open_out file in
  try
      pretty_to_channel oc (`List instances) ;
      close_out oc
  with e ->
      close_out_noerr oc;
      raise e
  (* to_file ~suf:"" file (`List instances) *)