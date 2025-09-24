
(* ========= SIGNATURES & ANNOTATIONS ========= *)

val valint : int
let valint = true
let valint = 42

val test_sig : 'a|bool -> 'a|int -> ('a|bool,'a|int)
let test_sig x y = (y,x)
let test_sig x y = (x,y)

val test_sig_overloaded : (int -> int) & (bool -> bool)
let test_sig_overloaded x = x

let test_annot (x:'a|bool) (y:'a|int) = (x,y)

(* ========= SIMPLE CONSTRUCTOR TESTS ======== *)

let proj_a (A(v)) = v
let proj_ab x =
  match x with
  | A(v) -> v
  | B(v) -> v
  end

let proj_ab_test x y = (proj_ab A(x), proj_ab B(y))

type clist('a) = Nil | Cons('a, clist('a))
let map_clist f (lst:clist('a)) =
  match lst with
  | Cons(v,tail) -> Cons(f v, map_clist f tail)
  | Nil -> Nil
  end

let record_ab_open ({ a ; b ..}) = { a ; b }
let record_ab ({ a ; b }) = { a ; b }

(* ========= OPAQUE TYPES & IMPERATIVE PROGRAMMING ======== *)

val (<) : int -> int -> bool
val (<=) : int -> int -> bool
val (>) : int -> int -> bool
val (>=) : int -> int -> bool

abstract type ref('a)
val ref : 'a -> ref('a)
val (<-) : ref('a) -> 'a -> ()
val (!) : ref('a) -> 'a

val ref_42 : ref(int)
let ref_42 = ref 42
let ref_42_unresolved = ref 42
let mutate_ref x =
  let y = ref x in
  y <- 42 ; !y

let is_ref x = if x is ref then true else false
let is_not_ref x = if x is ~ref then true else false

abstract type dict('k, 'v)
abstract type array('a)

val dict : () -> dict('a, 'b)
val array : () -> array('a)
val ([]<-) : ((dict('a, 'b), 'a) -> 'b -> ()) & ((array('b), int) -> 'b -> ())
val ([]) : ((dict('a, 'b), 'a) -> 'b) & ((array('b), int) -> 'b)
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

(* val swap : ('a -> 'a -> dict('a,'b) -> ())
         & (int -> int -> array('b) -> ()) *)
let swap i j x =
    let tmp = x[i] in
    x[i]<- x[j] ; x[j]<- tmp


(* =========== TYPE NARROWING ============== *)

val succ : int -> int

let impossible_branch = fun x ->
  if x is int then x + 1 else (42 3)

let impossible_branch2 = fun x -> fun y ->
  if y is int then y+1 else x+1

let switch1 f s a b =
  if s then f a else f b

let switch2 s f a b =
  if s then f a else f b

let typeof x =
  if x is ()|[] then "[]"
  else if x is string then "string"
  else if x is char then "char"
  else if x is int then "Number"
  else if x is bool then "Boolean"
  else "Object"

let lnot = fun a ->
  if a is true then false else true

let lor = fun a -> fun b ->
  if a is false then if b is false then false else true else true

let land = fun a -> fun b ->
  if a is true then if b is false then false else true else false

let tautology = fun x -> fun y ->
  land (lor x (lnot x)) (lor (lnot y) y)

let tautology_ann = fun (x:any) -> fun (y:any) ->
  suggest x is true or ~true in
  suggest y is true or ~true in
  land (lor x (lnot x)) (lor (lnot y) y)

let test_many_params_ann1 (a:any) (b:any) (c:any) (d:any) (e:any) (f:any) =
  if (a,b,c,d,e,f) is (int,bool,int,bool,int,bool) then (a,b,c,d,e,f) else false

let test_many_params_ann2 (a:'a) (b:'b) (c:'c) (d:'d) (e:'e) (f:'f) =
  if (a,b,c,d,e,f) is (int,bool,int,bool,int,bool) then (a,b,c,d,e,f) else false

let test_many_params a b c d e f =
  if (a,b,c,d,e,f) is (int,bool,int,bool,int,bool) then (a,b,c,d,e,f) else false

let match_pair (x:any) (y:any) =
  match (x,y) with
  | :(int, bool) -> (x + 1, lnot y)
  | _ -> false
  end

let type_narrowing_fail (f:any -> any) x =
  if f x is int then (f x) + 1 else 0

let type_narrowing_ok (f:any -> any) x =
  let y = f x in
  if y is int then y + 1 else 0

let type_narrowing2_ok (f:(any -> any) & ('a -> false)) (x:any) =
  if f x then x else 42

let type_narrowing2_ok' ((f,x): ((any -> any) & ('a -> false), any)) =
  if f x then x else 42

(* ===== bal(ance) from OCaml Map module ===== *)

val invalid_arg : string -> empty

type t('a) =
  Nil | (t('a), Key, 'a, t('a), int)

let height (x: t('a)) =
  match x with
  | :Nil -> 0
  | (_,_,_,_,h) -> h
  end

let create l x d r =
  let hl = height l in
  let hr = height r in
  (l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

(* #type_narrowing = false *)

val bal : t('a) -> Key -> 'a -> t('a) -> t('a)
let bal l x d r =
(* let bal (l:t('a)) (x: Key) (d:'a) (r:t('a)) : t('a) = *)
  let hl = match l with :Nil -> 0 | (_,_,_,_,h) -> h end in
  let hr = match r with :Nil -> 0 | (_,_,_,_,h) -> h end in
  if hl > (hr + 2) then
    match l with
    | :Nil -> invalid_arg "Map.bal"
    | (ll, lv, ld, lr, _) ->
      if (height ll) >= (height lr) then
        create ll lv ld (create lr x d r)
      else
        match lr with
        | :Nil -> invalid_arg "Map.bal"
        | (lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
        end
    end
  else if hr > (hl + 2) then
    match r with
    | :Nil -> invalid_arg "Map.bal"
    | (rl, rv, rd, rr, _) ->
      if (height rr) >= (height rl) then
        create (create l x d rl) rv rd rr
      else
        match rl with
        | :Nil -> invalid_arg "Map.bal"
        | (rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
        end
    end
  else (l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

(*************************************************
*    Examples from Tobin-Hochstadt & Felleisen   *
*     exampleX = EXPLICITLY ANNOTATED VERSIONS   *
*     implictX = IMPLICITLY ANNOTATED VERSIONS   *
*                                                *
**************************************************)

(*
 Interesting points:
  - example6: not typable with the annotation int|string
    (as expected), but if we remove annotations becomes typable.
    That is our system finds the right constraints to make the
    expression typable
  - in examples 10 11 12 we do not have to assume that p is
    a product, the system deduces it alone
  - same for the example 14. We do not have to assume that
    the parameter input is int|string and extra is a pair. The system
    finds it alone and it works for user defined "and"
*)

(* Prelude *)

let and_ = fun (x, y) ->
     if x is true then if y is true then x else false else false
let not_ = fun x -> if x is true then false else true
let or_ =  fun (x,y) ->
  not_ (and_ (not_ x, not_ y))

let is_string = fun x ->
     if x is string then true else false
let is_int = fun x ->
     if x is int then true else false

val strlen : string -> int
val add : int -> int -> int
val add1 : int -> int
val f : (int | string) -> int
val g : (int, int) -> int

(* Examples Tobin-Hochstadt & Felleisen *)

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


let example6 = fun (x : int|string) -> fun (y : any) ->
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

(* TODO: fix *)
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

(*******************************
 *                             *
 *  Examples for polymorphism  *
 *                             *
 *******************************)

type falsy = false | "" | 0
type truthy = ~falsy

let test a = (fst a, fst a)

let and_js = fun x -> fun y ->
  if x is falsy then x else y

let not_js = fun x -> if x is falsy then 1 else 0

let or_js = fun x -> fun y ->
  if x is truthy then x else y

let identity_js = fun x -> or_js x x

let and_pair = fun x -> fun y ->
  if x is falsy then x else (y, succ x)

(* val test_pair : ((int \ 0, any) | (int, int) -> int) *)
let test_pair = fun x ->
  if fst x is falsy then (fst x) + (snd x) else succ (fst x)

type tt('a, 'b)  =  'a -> 'b -> 'a
type ff('a, 'b)  =  'a -> 'b -> 'b

val ifthenelse : (tt('c, 'd) -> 'c -> 'd -> 'c) & (ff('c, 'd) -> 'c -> 'd -> 'd)
let ifthenelse b x y = b x y

(* Pattern matching *)

let test1_patterns (a,_) = a

let test2_patterns x =
  match x with (a,_)&(_,b) -> (a,b) end

let test3_patterns x y =
  let pack x y = (x,y) in
  let (y,x) = pack x y in
  pack x y

let typeof_patterns x =
  match x with
  | :() | :[] -> "[]"
  | :string -> "string"
  | :char -> "char"
  | :int -> "Number"
  | :bool -> "Boolean"
  | :any -> "Object"
  end

let land_patterns a b =
  match a,b with
  | true, true -> true
  | :any -> false
  end

(*************************************
 *                                   *
 *  Examples of recursive functions  *
 *                                   *
 *************************************)

(* ============== FIXPOINT COMBINATOR ============= *)

(* The type deduced for fixpoint can be read as follows
   forall('c <: 'a -> 'b)('d <:'c). ('c -> 'd) -> 'd 
*)
let fixpoint = fun f ->
  let delta = fun x ->
     f ( fun  v -> ( x x v ))
   in delta delta

let fact_stub fact n =
  if n is 0 then 1 else (fact (n-1))*n

let fact = fixpoint fact_stub

let length_stub length lst =
  if lst is [] then 0 else succ (length (tl lst))

let length = fixpoint length_stub

let map_stub map f lst =
  if lst is [] then []
  else (f (hd lst))::(map f (tl lst))

let map x = fixpoint map_stub x

(* ========= RECURSIVE FUNCTIONS ========= *)

let fact_rec (x:int) =
  if x is 0 then 1 else x * (fact_rec (x-1))

let map_rec f (lst:['a*]) =
  match lst with
  | [] -> []
  | a::lst -> (f a)::(map_rec f lst)
  end

let map_noannot f lst =
  match lst with
  | [] -> []
  | a::lst -> (f a)::(map_noannot f lst)
  end

let foo x = bar x
and bar y = foo y

val filter_rec : ('a->any) & ('b -> ~true) -> [('a|'b)*] -> [('a\'b)*]
let filter_rec f l =
  match l with
  | [] -> []
  | e::l ->
    if f e is true
    then e::(filter_rec f l)
    else filter_rec f l
  end

let filter_rec2 (f: ('a->any) & ('b -> ~true)) (l:[('a|'b)*]) =
  match l with
  | [] -> []
  | e::l ->
    if f e is true
    then e::(filter_rec2 f l)
    else filter_rec2 f l
  end

let filter_noannot f l =
  match l with
  | [] -> []
  | e::l ->
    let l = filter_noannot f l in
    if f e is true then e::l else l
  end

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

type objF('a) = { f :? 'a ; proto :? objF('a) ..}

let call_f (o:objF('a)) =
  if o is { f : any ..} then o.f
  else if o is { proto : any ..}
  then call_f o.proto
  else ()

(* ========= COMPLEX RECURSIVE FUNCTIONS ========= *)

val concat : ['a*] -> ['b*] -> ['a* 'b*]
let concat (x:['a*]) (y:['b*]) =
   if x is [] then y else (hd x)::(concat (tl x) y)

let flatten_ocaml (x:[['a*]*])  =
  if x is [] then [] else concat (hd x) (flatten_ocaml (tl x))

let reverse (l:[['a*]*]) =
  if l is [] then [] else concat (reverse (tl l)) [hd l]

type expr = ("const", (0..)) | ("add", (expr, expr)) | ("uminus", expr)

let eval (e:expr) =
  match e with
  | (:"add", (e1, e2)) -> (eval e1) + (eval e2)
  | (:"uminus", e) -> 0 - (eval e)
  | (:"const", x) -> x
  end

let mapi_aux (i:int) f (l:['a*]) =
  match l with
  | [] -> []
  | x::ll -> let r = f i x in r::(mapi_aux (i+1) f ll)
  end

let mapi f l = mapi_aux 0 f l

let rec_and_imp arr k i n =
  if k < n do arr[k]<- (i+k) ; rec_and_imp arr (k+1) i n end

let interval i j =
  let arr = array () in
  rec_and_imp arr 0 i ((j-i)+1) ; arr

(* ========= GRADUAL ========= *)

val reflect

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

(* ========= INFERENCE OF NON-OVERLOADED TYPES ========= *)

# infer_overload = false

let eval2 e =
  match e with
  | (:"add", (e1, e2)) -> (eval2 e1) + (eval2 e2)
  | (:"uminus", e) -> 0 - (eval2 e)
  | (:"const", x) -> x
  end

let map_noannot2 f lst =
  match lst with
  | [] -> []
  | a::lst -> (f a)::(map_noannot2 f lst)
  end

# infer_overload = true

(* ========= CONTROL FLOW ========= *)

let typeof_cf x =
  if x is string do return "string" end ;
  if x is bool do return "bool" end ;
  if x is int do return "int" end ;
  if x is char do return "char" end ;
  if x is () do return "unit" end ;
  "object"

let break_return_cf x =
  while x is ~false do
    if x is 0 do break end ;
    return x
  end

let weird_return x =
  42, (if x then return 69 else 69)

(* ========= MUTABLE VARS ========= *)

let mut mx : int = 42
let mut_invalid =
  mx := true ; mx
let mut_valid =
  mx := 69 ; mx

val mut my
let mut my = 42
let read_mut = my
let read_mut_cast = (my :>> bool)
let write_mut = my := false

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
