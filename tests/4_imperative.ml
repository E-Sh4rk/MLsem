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

val succ : int -> int

(* ========= References encoding ========= *)

abstract type ref('a)
val ref : 'a -> ref('a)
val (<-) : (ref('a), 'a) -> ()
val (!) : ref('a) -> 'a

val ref_42 : ref(int)
let ref_42 = ref 42
let ref_42_unresolved = ref 42
let mutate_ref x =
  let y = ref x in
  y <- 42 ; !y

let is_ref x = if x is ref then true else false
let is_not_ref x = if x is ~ref then true else false

(* ========= Dict and arrays ========= *)

abstract type dict('k, 'v)
abstract type array('a)

val dict : () -> dict('a, 'b)
val array : () -> array('a)
val ([]<-) : (dict('a, 'b), 'a, 'b) -> ()
val ([]<-) : (array('b), int, 'b) -> ()
val ([]) : (dict('a, 'b), 'a) -> 'b
val ([]) : (array('b), int) -> 'b
val push : array('a) -> 'a -> ()
val len : array('a) -> int

let test_dict x =
  let d = dict () in
  d[x]<- 42 ;
  d["key"]<- 0 ;
  d, d[false]

let filter_arr (f:('a -> any) & ('b -> ~true)) (arr:array('a|'b)) =
  let res = array () in
  let i = ref 0 in
  while !i < (len arr) do
    let e = arr[!i] in
    if f e do push res e end ;
    i <- (!i + 1)
  end ;
  res

(* val test_arr : 'a -> array('a | 'b) *)
let test_arr x =
  let arr = array () in
  push arr true ;
  push arr x ;
  push arr false ;
  filter_arr (fun x -> if x is int then true else false) arr

let test_double_array =
  let arr = array () in
  arr[0]<- (array ()) ;
  (arr[0])[0]<- 42 ;
  (arr[0])[0]

(* val arr_dict_assign: (array('a)|dict(int,'a) -> ()) *)
let arr_dict_assign x = x[0]<- x[1]

let nested x y =
  let d = dict () in
  d[x]<- (array ()) ;
  (d[x])[0]<- y ; (d[x])[0]

(* val swap : 'a -> 'a -> dict('a,'b) -> () &&
              int -> int -> array('b) -> () *)
let swap i j x =
    let tmp = x[i] in
    x[i]<- x[j] ; x[j]<- tmp

let mut mx : int = 42
let mut_invalid =
  mx := true ; mx
let mut_valid =
  mx := 69 ; mx

(* ========= Type narrowing and complex control flow ========= *)

let mut counter : int = 0
let incr x = counter := succ counter
let test_global =
    counter := 0 ;
    incr () ;
    counter (* TODO: fix wrong assumption that counter cannot be modified by incr *)

let mut_narrowing =
  let mut y = 42 in
  while y is ~Nil do
    y := succ y ;
    (* ... *)
    if y > 100 do y := Nil end
  end ;
  y

let mut_seq1 =
  let mut y = false in
  while <bool> do y := false end ;
  y := 42 ;
  while <bool> do y := 42 end ;
  y

let mut_seq2 =
  let mut y = false in
  while <bool> do y := false end ;
  y := 42 ;
  while <bool> do y := 42 end ;
  y := Nil ;
  while <bool> do y := Nil end ;
  y

let mut_and_return (_) =
    if mx is 0 do
        mx := 69
    else
        mx := 0 ; return false
    end ;
    mx

let neg_and_pos x =
  let mut x = x in
  if x is Nil do return x end ;
  if x < 0 do x := 0-x end ;
  x := (0-x,x) ;
  return x

let neg_and_pos_ann (x:int|Nil) =
  let mut x = x in
  if x is Nil do return x end ;
  if x < 0 do x := 0-x end ;
  x := (0-x,x) ;
  return x

let order (x:(int|Nil,int|Nil)|Nil) =
  let mut x = x in
  if x is Nil do return x end ;
  if fst x is Nil do return snd x end ;
  if snd x is Nil do return fst x end ;
  if (snd x) < (fst x) do x := (snd x, fst x) end ;
  return x

val rand : () -> any
val is_int : (int -> true) & (~int -> false)
let loop_tricky_narrowing y =
  let mut x in
  let mut y = y in
  while is_int (x := rand () ; x) do
    y := y + x
  end ;
  return (x,y)

let loop_invalid x =
  let mut x = x in
  while true do
    x := x + 1 ;
    x := false
  end ;
  x

let loop_valid x =
  let mut x = x in
  while true do
    if x is ~int do return x end ;
    x := x + 1 ;
    x := false
  end ;
  x

let filter_imp (f:('a -> bool) & ('b -> false)) (arr:array('a|'b)) =
  let res = array () in
  let mut i = 0 in
  while i < (len arr) do
    let e = arr[i] in
    if f e do push res e end ;
    i := i + 1
  end ;
  return res

val filter_imp_test : array(42|13|"abc")
(* we have to specify a type for filter_imp_test,
   otherwise the type inference will infer array(42|13|"abc"|'a) where 'a cannot be generalized,
   which will error as top-level definitions cannot have unquantified type variables *)
let filter_imp_test =
  let mut arr = array () in
  push arr 42 ;
  push arr false ;
  push arr 13 ;
  arr := filter_imp (fun x -> (x is int)) arr ;
  push arr "abc" ;
  arr

let rec_and_imp arr k i n =
  if k < n do arr[k]<- (i+k) ; rec_and_imp arr (k+1) i n end

let interval i j =
  let arr = array () in
  rec_and_imp arr 0 i ((j-i)+1) ; arr
