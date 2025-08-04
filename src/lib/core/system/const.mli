open Types

type t =
| Unit | Nil
| EmptyRecord
| Bool of bool
| Int of Z.t
| Float of float
| Char of char
| String of string

val typeof : t -> Ty.t

val pp : Format.formatter -> t -> unit
