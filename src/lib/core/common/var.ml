module Variable = struct

  let data : (int, string option * Position.t) Hashtbl.t = Hashtbl.create 100

  type t = int
  let compare = Int.compare
  let equal = Int.equal

  let next_id =
    let last = ref 0 in
    fun () ->
      last := !last + 1 ;
      !last

  let create display_name =
    let id = next_id () in
    Hashtbl.add data id (display_name, Position.dummy) ;
    id

  let refresh id =
    let (name, loc) = Hashtbl.find data id in
    let id = next_id () in
    Hashtbl.add data id (name, loc) ;
    id

  let attach_location id loc =
    let (name, _) = Hashtbl.find data id in
    Hashtbl.replace data id (name, loc)

  let get_location id =
    let (_, loc) = Hashtbl.find data id
    in loc

  let get_name id =
    let (name, _) = Hashtbl.find data id in
    name
    
  let show t =
    match get_name t with
    | None -> string_of_int t
    | Some str -> str

  let show_uniq id =
    let prefix = match get_name id with None -> "" | Some str -> str in
    prefix^"_"^(string_of_int id)

  let pp fmt t = Format.fprintf fmt "%s" (show t)
  let pp_uniq fmt t = Format.fprintf fmt "%s" (show_uniq t)
end

module VarMap = Map.Make(Variable)
module VarSet = Set.Make(Variable)