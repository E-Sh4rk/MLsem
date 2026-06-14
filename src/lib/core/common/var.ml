module Variable = struct

  type data = { name:string option ; pos:Position.t ; sig_pos:Position.t list }
  let data : (int, data) Hashtbl.t = Hashtbl.create 100

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
    Hashtbl.add data id { name=display_name ; pos=Position.dummy ; sig_pos=[] } ;
    id

  let refresh id =
    let v = Hashtbl.find data id in
    let id = next_id () in
    Hashtbl.add data id v ;
    id

  let attach_location id loc =
    let v = Hashtbl.find data id in
    Hashtbl.replace data id { v with pos=loc }

  let attach_sig_location id loc =
    let v = Hashtbl.find data id in
    Hashtbl.replace data id { v with sig_pos=loc::v.sig_pos }

  let get_location id = (Hashtbl.find data id).pos

  let get_sig_locations id = (Hashtbl.find data id).sig_pos

  let get_name id = (Hashtbl.find data id).name
    
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