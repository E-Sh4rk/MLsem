
type t = int
module Set = Set.Make(Int)
module Map = Map.Make(Int)
let dummy = 0
let unique_id =
    let last_id = ref 0 in
    fun () -> (
        last_id := !last_id + 1 ;
        !last_id
    )

type info = { loc : Position.t ; show_notices : bool ; eq_class : Set.t ref }
let eid_infos = Hashtbl.create 1000
(* [dummy] must have an entry like any other id, so that the accessors below are total on it. *)
let () = Hashtbl.add eid_infos dummy
  { loc=Position.dummy ; show_notices=false ; eq_class = ref (Set.singleton dummy) }
let mk loc show_notices =
  let eid = unique_id () in
  Hashtbl.add eid_infos eid { loc ; show_notices ; eq_class = ref (Set.singleton eid) } ;
  eid
let unique_with_pos loc = mk loc true
let generated_with_pos loc = mk loc false
let unique () = mk Position.dummy false

let refresh parent =
  if Int.equal parent dummy then dummy
  else
    let info = Hashtbl.find eid_infos parent in
    let eid = unique_id () in
    info.eq_class := Set.add eid !(info.eq_class) ;
    Hashtbl.add eid_infos eid info ; eid
let eq_class t = !((Hashtbl.find eid_infos t).eq_class)

let loc eid = (Hashtbl.find eid_infos eid).loc
let show_notices eid = (Hashtbl.find eid_infos eid).show_notices

let equal, compare, hash = Int.equal, Int.compare, Int.hash
let pp fmt t = Format.fprintf fmt "%i" t