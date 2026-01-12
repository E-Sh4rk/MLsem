open Tvar
open Base
open Yojson.Safe
open Recording_internal

let start_recording () = recording := true
let stop_recording () = recording := false

type tally_call = Recording_internal.tally_call
let clear () = tally_calls := []
let tally_calls () = List.rev !tally_calls

let save_to_file file tallys =
    let ty_to_string ty = `String (Format.asprintf "@[<h>%a@]" Ty.pp_raw ty) in
    let instances = tallys |> List.map (fun (r:tally_call) ->
        let s = TVOp.shorten_names (MVarSet.of_list r.vars1 r.vars2) in
        let ty_to_string ty = Subst.apply s ty |> ty_to_string in
        let to_str1 = List.map (fun v ->
            match Subst.find1 s v |> TVOp.vars |> MVarSet.proj1 |> TVarSet.elements with
            | [r] -> `String (TVar.name r) | _ -> assert false) in
        let to_str2 = List.map (fun v ->
            match Subst.find2 s v |> Row.row_vars |> RVarSet.elements with
            | [r] -> `String (RVar.name r) | _ -> assert false) in
        let vars1, vars2, mono1, mono2 =
            to_str1 r.vars1, to_str2 r.vars2, to_str1 r.mono1, to_str2 r.mono2 in
        let cs = r.constraints |> List.map (fun (s,t) ->
            `List  [ty_to_string s ; ty_to_string t]
            )
        in
        `Assoc [ ("vars", `List vars1) ; ("mono", `List mono1) ;
                 ("rvars", `List vars2) ; ("rmono", `List mono2) ; ("constr", `List cs) ]
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
