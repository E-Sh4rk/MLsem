
type t = int
let dummy = 0
let unique_id =
    let last_id = ref 0 in
    fun () -> (
        last_id := !last_id + 1 ;
        !last_id
    )

type info = { loc : Position.t ; show_notices : bool }
let eid_infos = Hashtbl.create 1000
let unique_with_pos pos =
  let eid = unique_id () in
  Hashtbl.add eid_infos eid { loc=pos ; show_notices=true } ; eid
let unique () =
  let eid = unique_id () in
  Hashtbl.add eid_infos eid { loc=Position.dummy ; show_notices=false } ; eid
let refresh parent =
  if Int.equal parent dummy then dummy
  else
    let info = Hashtbl.find eid_infos parent in
    let eid = unique_id () in
    Hashtbl.add eid_infos eid info ; eid
let loc eid = (Hashtbl.find eid_infos eid).loc
let show_notices eid = (Hashtbl.find eid_infos eid).show_notices
let set_show_notices eid b =
  let info = Hashtbl.find eid_infos eid in
  Hashtbl.replace eid_infos eid { info with show_notices=b }
let equal, compare, hash = Int.equal, Int.compare, Int.hash
let pp fmt t = Format.fprintf fmt "%i" t