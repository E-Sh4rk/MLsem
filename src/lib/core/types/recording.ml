
let recording = ref false
let start_recording () = recording := true
let stop_recording () = recording := false

(* === Recording of tallying calls === *)
type tally_call = {
    vars : Sstt.Var.t list ;
    mono : Sstt.Var.t list ;
    priority : Sstt.Var.t list ;
    constraints : (Sstt.Ty.t * Sstt.Ty.t) list
    }
let tally_calls = ref []
let record mono priority cs =
  if !recording then begin
    let vars (s,t) = Sstt.VarSet.union (Sstt.Ty.vars s) (Sstt.Ty.vars t) in
    let tvars = List.fold_left
      (fun acc c -> Sstt.VarSet.union acc (vars c)) Sstt.VarSet.empty cs in
    let order = Sstt.VarSet.elements tvars in
    let priority = priority |> List.filter (fun v -> Sstt.VarSet.mem v tvars) in
    let mono = Sstt.VarSet.inter tvars mono |> Sstt.VarSet.elements in
    tally_calls := {vars=order;mono;priority;constraints=cs}::!tally_calls
  end
let clear () = tally_calls := []
let tally_calls () = !tally_calls
