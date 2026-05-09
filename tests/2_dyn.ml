val (+) : (int, int) -> int
val (-) : (int, int) -> int
val ( * ) : (int, int) -> int
val (/) : (int, int) -> int
val (%) : (int, int) -> int
val (@) : (['a*], ['b*]) -> ['a* 'b*]

val (<) : (int, int) -> bool
val (<=) : (int, int) -> bool
val (>) : (int, int) -> bool
val (>=) : (int, int) -> bool

val lnot : (true -> false) & (false -> true)
val reflect : dyn

let test_reflect x = reflect x

val test_reflect_ann : int -> int
let test_reflect_ann x = reflect x

let gradual1 x =
  match reflect x with
  | y & :int -> y + 1
  | y & :bool -> lnot y
  end

let gradual2 x =
  match reflect x with
  | y & :int -> y
  | y & :bool -> y
  end

let gradual3 x =
  match reflect x with
  | y & :int -> y + 1
  | y & :bool -> lnot y
  | y -> y
  end

let gradual4 x =
  match reflect x with
  | y & :int -> y + 1
  | y & :bool -> lnot y
  | y -> 42::y
  end

val gradual4_ann : any -> int | bool
let gradual4_ann x =
  match reflect x with
  | y & :int -> y + 1
  | y & :bool -> lnot y
  | y -> 42::y
  end

val lkp : ['a*] -> 'a | Nil & dyn

val test_lkp_ok : int
let test_lkp_ok =
    let elt = lkp [1;2;3;4;5] in
    elt + 1

let test_lkp_fail =
    let elt = lkp [1;2;3;4;5] in
    lnot elt

(* Mutable var *)

val mut mx : dyn
let mut mx = 42
let read_mut = mx
let read_mut_cast = (mx :>! bool)
let write_mut = mx := false
