(* ============================= *)
(* ========== Prelude ========== *)
(* ============================= *)

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

val rand : () -> any
val is_int : (int -> true) & (~int -> false)
val is_string : (string -> true) & (~string -> false)
val strlen : string -> int
val add : int -> int -> int
val add1 : int -> int
val invalid_arg : string -> empty

(* ============================================ *)
(* ========== Imperative definitions ========== *)
(* ============================================ *)
(* Imperative function definitions from Figure 6 *)
(* Note: unlike the paper, we use uncurified types for binary operators *)

abstract type dict('k, 'v)
abstract type array('a)

val dict : () -> dict('a, 'b)
val array : () -> array('a)
val ([]<-) : ((dict('a, 'b), 'a, 'b) -> ()) & ((array('b), int, 'b) -> ())
val ([]) : ((dict('a, 'b), 'a) -> 'b) & ((array('b), int) -> 'b)
val push : array('a) -> 'a -> ()
val len : array('a) -> int

(* ====================================== *)
(* ========== Filter functions ========== *)
(* ====================================== *)
(* Examples from the introduction *)

let filter (f:('a -> bool) & ('b -> false)) (l:[('a|'b)*]) =
  match l with
  | [] -> []
  | e::l -> if f e then e::(filter f l) else filter f l
  end

let test_filter = filter (fun x -> (x is int)) [42 ; Null ; true ; 33]

let filter_imp (f:('a -> bool) & ('b -> false)) (arr:array('a|'b)) =
  let res = array () in
  let mut i = 0 in
  while i < (len arr) do
    let e = arr[i] in
    if f e do push res e end ;
    i := i + 1
  end ;
  return res

val filtermap :
    (('t -> ((true, 'u) | false), ['t*]) -> ['u*])
  & (('t -> ((true, 'u) | bool), ['t*]) -> [('t | 'u)*])
let filtermap (f, l) =
    match l with
    | [] -> []
    | x::xs ->
        match f x with
        | false -> filtermap (f, xs)
        | true -> x::(filtermap (f, xs))
        | (true, y) -> y::(filtermap (f, xs))
        end
    end

(* ========================================== *)
(* ========== Imperative functions ========== *)
(* ========================================== *)
(* Examples from Section 5, Figures 5 and 6 *)

let neg_and_pos x =
  let mut x = x in
  if x is Nil do return x end ;
  if x < 0 do x := 0-x end ;
  x := (0-x,x) ;
  return x

val rand_any : () -> any

let loop_type_narrowing y =
  let mut x in
  let mut y = y in
  while is_int
    (x := rand_any () ; x) do
    y := y + x
  end ;
  return (x,y)

let loop_invalid x =
  let mut x = x in while true do
    x := x + 1 ; x := false
  end ; x

let loop_valid x =
  let mut x = x in while true do
    if x is ~int do return x end ;
    x := x + 1 ; x := false
  end ; x

let nested x y =
  let d = dict () in
  d[x]<- (array ()) ;
  (d[x])[0]<- y ; (d[x])[0]

let swap i j x =
  let tmp = x[i] in
  x[i]<- x[j] ; x[j]<- tmp

(* ======================================== *)
(* ========== Bal(ance) function ========== *)
(* ======================================== *)
(* Mentioned in Section 5 *)

type t('a) =
  Nil | Node(t('a), Key, 'a, t('a), int)

let height (x: t('a)) =
  match x with
  | Nil -> 0
  | Node(_,_,_,_,h) -> h
  end

let create l x d r =
  let hl = height l in
  let hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

val bal : t('a) -> Key -> 'a -> t('a) -> t('a)
let bal l x d r =
(* let bal (l:t('a)) (x: Key) (d:'a) (r:t('a)) : t('a) = *)
  let hl = match l with Nil -> 0 | Node(_,_,_,_,h) -> h end in
  let hr = match r with Nil -> 0 | Node(_,_,_,_,h) -> h end in
  if hl > (hr + 2) then
    match l with
    | Nil -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
      if (height ll) >= (height lr) then
        create ll lv ld (create lr x d r)
      else
        match lr with
        | Nil -> invalid_arg "Map.bal"
        | Node(lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
        end
    end
  else if hr > (hl + 2) then
    match r with
    | Nil -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
      if (height rr) >= (height rl) then
        create (create l x d rl) rv rd rr
      else
        match rl with
        | Nil -> invalid_arg "Map.bal"
        | Node(rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
        end
    end
  else Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

(* ==================================== *)
(* ========== Type narrowing ========== *)
(* ==================================== *)
(* Examples from [Tobin-Hochstadt and Felleisen 2010] mentioned in Section 5 *)

val f : (int | string) -> int
val g : (int, int) -> int

let and_ (x,y) =
  match x,y with
  | true,true -> true
  | _ -> false
  end
let not_ x = if x is true then false else true
let or_ = fun (x,y) -> not_ (and_ (not_ x, not_ y))

let example1 = fun (x:any) ->
  if x is int then add1 x else 0

let implicit1 = fun x ->
  if x is int then add1 x else 0


let example2 = fun (x:string|int) ->
  if x is int then add1 x else strlen x

let implicit2 = fun x ->
  if x is int then add1 x else strlen x


let example3 = fun (x: any) ->
  if x is (any \ false) then (x,x) else false

let implicit3 = fun x ->
  if x is (any \ false) then (x,x) else false


let example4 = fun (x : any) ->
  if or_ (is_int x, is_string x) is true then x else 'A'

let implicit4 = fun x ->
  if or_ (is_int x, is_string x) is true then x else 'A'


let example5 = fun (x : any) -> fun (y : any) ->
  if and_ (is_int x, is_string y) is true then
   add x (strlen y) else 0

let implicit5 = fun x -> fun y ->
  if and_ (is_int x, is_string y) is true then
   add x (strlen y) else 0

(* Annotations for this one are invalid: y can be any only if x is string *)
let example6_invalid = fun (x : int|string) -> fun (y : any) ->
  if and_ (is_int x, is_string y) is true then
   add  x (strlen y) else strlen x

val example6 : (int -> string -> int) & (string -> any -> int)
let example6 = fun x -> fun y ->
  if and_ (is_int x, is_string y) is true then
   add  x (strlen y) else strlen x

let implicit6 = fun x -> fun y ->
  if and_ (is_int x, is_string y) is true then
   add  x (strlen y) else strlen x


let example7 = fun (x : any) -> fun (y : any) ->
  if (if is_int x is true then is_string y else false) is true then
   add x (strlen y) else 0

let implicit7 = fun x -> fun y ->
  if (if is_int x is true then is_string y else false) is true then
   add x (strlen y) else 0


let example8 = fun (x : any) ->
  if or_ (is_int x, is_string x) is true then true else false

let implicit8 = fun x ->
  if or_ (is_int x, is_string x) is true then true else false


let example9 = fun (x : any) ->
  if
   (if is_int x is true then is_int x else is_string x)
   is true then  f x else 0

let implicit9 = fun x  ->
  if
   (if is_int x is true then is_int x else is_string x)
   is true then  f x else 0


let example10 = fun (p : (any,any)) ->
  if is_int (fst p) is true then add1 (fst p) else 7

let implicit10 = fun p ->
  if is_int (fst p) is true then add1 (fst p) else 7

let example11 = fun (p : (any, any)) ->
  if and_ (is_int (fst p), is_int (snd p)) is true then g p else No

let implicit11 = fun p ->
  if and_ (is_int (fst p), is_int (snd p)) is true then g p else No

let example12 = fun (p : (any, any)) ->
  if is_int (fst p) is true then true else false

let implicit12 = fun p ->
  if is_int (fst p) is true then true else false


let example13 =
 fun (x : any) ->
   fun (y : any) ->
    if and_ (is_int x, is_string y) is true then 1
    else if is_int x is true then 2
    else 3

let implicit13 =
 fun x ->
   fun y ->
    if and_ (is_int x, is_string y) is true then 1
    else if is_int x is true then 2
    else 3

let example14 = fun (input : int|string) ->
  fun (extra : (any, any)) ->
    if and_(is_int input , is_int(fst extra)) is true then
        add input (fst extra)
    else if is_int(fst extra) is true then
        add (strlen input) (fst extra)
    else 0

let implicit14 = fun input ->
  fun extra ->
    if and_(is_int input , is_int(fst extra)) is true then
        add input (fst extra)
    else if is_int(fst extra) is true then
        add (strlen input) (fst extra)
    else 0
