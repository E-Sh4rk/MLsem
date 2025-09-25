module Variable = struct

  let data : (int, string option * Position.t) Hashtbl.t = Hashtbl.create 100

  type t = int
  let compare = compare
  let equals a b = a = b

  let next_id =
    let last = ref 0 in
    fun () ->
      last := !last + 1 ;
      !last

  let create display_name =
    let id = next_id () in
    Hashtbl.add data id (display_name, Position.dummy) ;
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

  let get_unique_name id =
    let prefix = match get_name id with None -> "" | Some str -> str in
    prefix^"_"^(string_of_int id)

  let pp fmt t =
    match get_name t with
    | None -> Format.fprintf fmt "%d" t
    | Some str -> Format.fprintf fmt "%s" str
    
  let show t =
    match get_name t with
    | None -> string_of_int t
    | Some str -> str
end

module VarMap = Map.Make(Variable)
module VarSet = Set.Make(Variable)