
val (=) : (int,int) | (bool,bool) | (string,string) | (char,char) | (float,float) -> bool

#infer_overload = false
#type_narrowing = false

(* Ad-hoc Arithmetic (non overloaded) *)

let f' x =
  match x with
  | 0 -> 0 | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4
  | 5 -> 5 | 6 -> 6 | 7 -> 7 | 8 -> 8 | 9 -> 9
  | 10 -> 10 | 11 -> 11 | 12 -> 12 | 13 -> 13 | 14 -> 14
  | 15 -> 15 | 16 -> 16 | 17 -> 17 | 18 -> 18 | 19 -> 19
  | 20 -> 20 | 21 -> 21 | 22 -> 22 | 23 -> 23 | 24 -> 24
  | _ -> (-1)
  end

let test' (y:(10..15)) = f' y

let succ' x =
  match x with
  | 0 -> 1 | 1 -> 2 | 2 -> 3 | 3 -> 4 | 4 -> 5
  | 5 -> 6 | 6 -> 7 | 7 -> 8 | 8 -> 9 | 9 -> 10
  | 10 -> 11 | 11 -> 12 | 12 -> 13 | 13 -> 14 | 14 -> 15
  | 15 -> 16 | 16 -> 17 | 17 -> 18 | 18 -> 19 | 19 -> 20
  | 20 -> 21 | 21 -> 22 | 22 -> 23 | 23 -> 24 | 24 -> 25
  | _ -> 0
  end

let test2' (y:(5..15)) = succ' (succ' (succ' (succ' (succ' y))))

(* Ad-hoc Arithmetic (overloaded) *)

#infer_overload = true

let f x =
  match x with
  | 0 -> 0 | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4
  | 5 -> 5 | 6 -> 6 | 7 -> 7 | 8 -> 8 | 9 -> 9
  | 10 -> 10 | 11 -> 11 | 12 -> 12 | 13 -> 13 | 14 -> 14
  | 15 -> 15 | 16 -> 16 | 17 -> 17 | 18 -> 18 | 19 -> 19
  | 20 -> 20 | 21 -> 21 | 22 -> 22 | 23 -> 23 | 24 -> 24
  | _ -> (-1)
  end

let test (y:(10..15)) = f y

let succ x =
  match x with
  | 0 -> 1 | 1 -> 2 | 2 -> 3 | 3 -> 4 | 4 -> 5
  | 5 -> 6 | 6 -> 7 | 7 -> 8 | 8 -> 9 | 9 -> 10
  | 10 -> 11 | 11 -> 12 | 12 -> 13 | 13 -> 14 | 14 -> 15
  | 15 -> 16 | 16 -> 17 | 17 -> 18 | 18 -> 19 | 19 -> 20
  | 20 -> 21 | 21 -> 22 | 22 -> 23 | 23 -> 24 | 24 -> 25
  | _ -> 0
  end

let test2 (y:(5..15)) = succ (succ (succ (succ (succ y))))

(* Boolean operations *)

let to_bool x =
  match x with
  | 0 -> false
  | :int -> true
  | false -> false
  | true -> true
  | y & :float -> y = 0.0
  | "" -> false
  | :string -> true
  | '\000' -> false
  | :char -> true
  end

let land (x,y) =
  match (x,y) with
  | (true,true) -> true
  | _ -> false
  end

let (&&) (x,y) = land (to_bool x, to_bool y)

let and1 = 42 && 0
let and2 = "" && true
let and3 = 42 && "42"
let and4 = 17.0 && 42.0
let and5 = 'c' && "c"

#infer_overload = true
#type_narrowing = true
