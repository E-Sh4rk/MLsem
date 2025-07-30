
type t = int
let dummy = 0
let unique =
    let last_id = ref 0 in
    fun () -> (
        last_id := !last_id + 1 ;
        !last_id
    )
let eid_locs = Hashtbl.create 1000
let unique_with_pos pos =
  let eid = unique () in
  Hashtbl.add eid_locs eid pos ; eid
let refresh parent =
  match Hashtbl.find_opt eid_locs parent with
  | None -> unique ()
  | Some pos -> unique_with_pos pos
let loc eid =
  match Hashtbl.find_opt eid_locs eid with
  | None -> Position.dummy
  | Some p -> p     
let pp fmt t = Format.fprintf fmt "%i" t