open Types

module Zd = struct
  type t = Z.t
  let pp = Z.pp_print
end
type t =
| Unit | Nil
| EmptyRecord
| Bool of bool
| Int of Zd.t
| Float of float
| Char of char
| String of string
[@@deriving show]

let typeof c =
  match c with
  | Unit -> Ty.unit
  | Nil -> Lst.nil
  | EmptyRecord -> Record.empty_closed
  | Bool true -> Ty.tt
  | Bool false -> Ty.ff
  | Int i -> Ty.interval (Some i) (Some i)
  | Float _ -> Ty.float
  | Char c -> Ty.char_interval c c
  | String str -> Ty.string_lit str
