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

(* Mutable var *)

let mut mx : dyn = 42
let read_mut = mx
let read_mut_cast = (mx :>! bool)
let write_mut = mx := false

(* Complex sigs using dyn *)

val lkp : ['a*] -> 'a | Nil & dyn

val test_lkp_ok : int
let test_lkp_ok =
    let elt = lkp [1;2;3;4;5] in
    elt + 1

let test_lkp_fail =
    let elt = lkp [1;2;3;4;5] in
    lnot elt

val magic_fail : dyn -> dyn
let magic_fail x = 42 42 ; x

val magic_ok : dyn -> dyn
let magic_ok x = x

val magic_ok2 : dyn|int -> dyn|int
let magic_ok2 x = x

(* Cast propagation *)

let test_cast1_ok x =
  (match x with :int -> 42 | x -> true end :> int)

let test_cast2_fail (x:any) =
  (match x with :int -> 42 | x -> true end :> int)

(* List examples *)

val last : [ any* 'a ] -> 'a
let test_first_last t = (hd t, last t)
let test_last_first t = (last t, hd t)
let test_first_second t = (hd t, hd (tl t))
let test_last t = last t

val test_first_last_ann : [ 'a any* ] & [ any* 'b ] -> ('a, 'b)
let test_first_last_ann t = (hd t, last t)

val test_first_last_ann2 : [ 'a | 'a any* 'b ] -> ('a, 'a | 'b)
let test_first_last_ann2 t = (hd t, last t)

let test_instance = test_first_last ("a" :: 1 :: [])
let test_instance_ann = test_first_last_ann ("a" :: 1 :: [])

(* Type narrowing *)

# type_narrowing = "partition"

val narr_dyn1 : dyn -> bool
let narr_dyn1 (x:dyn) =
  if x is int then true else false

val lkp : C('a) -> 'a | Nil & dyn
val narr_dyn2 : C(any) -> int
let narr_dyn2 x =
  if lkp x is int
  then lkp x
  else 0

val narr_dyn3 : C(~Nil) -> int
let narr_dyn3 x =
  if lkp x is int|Nil
  then lkp x
  else 0

# type_narrowing = "direct"

val narr_dyn1' : dyn -> bool
let narr_dyn1' (x:dyn) =
  if x is int then true else false

val narr_dyn2' : C(any) -> int
let narr_dyn2' x =
  if lkp x is int
  then lkp x
  else 0

val narr_dyn3' : C(~Nil) -> int
let narr_dyn3' x =
  if lkp x is int|Nil
  then lkp x
  else 0

(* Dyn with mutable variables *)

# type_narrowing = true

let t_if b x = let mut g = reflect x in if b do g := g end ; g
let t_seq x  = let mut g = reflect x in g := g ; g
let t_if_join b x = let mut g = 0 in if b do g := reflect x end ; g
let t_ann b x = let mut g : dyn = reflect x in if b do g := g end ; g
