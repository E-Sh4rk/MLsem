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

(* ========= SIGNATURES & ANNOTATIONS ========= *)

val valint : int
let valint = true
let valint = 42

val test_sig : 'a|bool -> 'a|int -> ('a|bool,'a|int)
let test_sig x y = (y,x)
let test_sig x y = (x,y)

val test_sig_overloaded : int -> int && bool -> bool
let test_sig_overloaded x = x

let test_annot (x:'a|bool) (y:'a|int) = (x,y)

(* ========= SIMPLE CONSTRUCTOR TESTS ======== *)

let proj_a (A(v)) = v
val proj_a' : A('a) -> (A('a)).A
let proj_a' v = v.A
let proj_ab x =
  match x with
  | A(v) -> v
  | B(v) -> v
  end

let proj_a_test x = proj_a A(x)
let proj_ab_test x y = (proj_ab A(x), proj_ab B(y))

type clist('a) = Nil | Cons('a, clist('a))
let map_clist f (lst:clist('a)) =
  match lst with
  | Cons(v,tail) -> Cons(f v, map_clist f tail)
  | Nil -> Nil
  end

let record_ab_open ({ a ; b ..}) = { a ; b }
let record_ab ({ a ; b }) = { a ; b }

(* ========= POLYMORPHISM ======== *)

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

val ifthenelse : tt('c, 'd) -> 'c -> 'd -> 'c &&
                 ff('c, 'd) -> 'c -> 'd -> 'd
let ifthenelse b x y = b x y

let test1_patterns (a,_) = a

let test2_patterns x =
  match x with (a,_)&(_,b) -> (a,b) end

let test3_patterns x y =
  let pack x y = (x,y) in
  let (y,x) = pack x y in
  pack x y

(* ========= RECURSIVE FUNCTIONS ========= *)

let fact (x:int) =
  if x is 0 then 1 else x * (fact (x-1))

let map f (lst:['a*]) =
  match lst with
  | [] -> []
  | a::lst -> (f a)::(map f lst)
  end

let map_noannot f lst =
  match lst with
  | [] -> []
  | a::lst -> (f a)::(map_noannot f lst)
  end

let foo x = bar x
and bar y = foo y

val filter : ('a->any) & ('b -> ~true) -> [('a|'b)*] -> [('a\'b)*]
let filter f l =
  match l with
  | [] -> []
  | e::l ->
    if f e is true
    then e::(filter f l)
    else filter f l
  end

let filter2 (f: ('a->any) & ('b -> ~true)) (l:[('a|'b)*]) =
  match l with
  | [] -> []
  | e::l ->
    if f e is true
    then e::(filter2 f l)
    else filter2 f l
  end

let filter_noannot f l =
  match l with
  | [] -> []
  | e::l ->
    let l = filter_noannot f l in
    if f e is true then e::l else l
  end

val filtermap : ('t -> ((true, 'u) | false), ['t*]) -> ['u*] &&
                ('t -> ((true, 'u) | bool), ['t*]) -> [('t | 'u)*]
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

val lor : ((true,bool)->true) & ((false, false) -> (false) ) & ((false, true) -> (true) )
val (==) : ((any,any)-> bool) & (('a, ~'a)-> false)

(* val member : ('a, ['a*])-> bool && (any, []) -> false && ('b,[(~'b)*]) -> false *)
let member (e,l) =
match l with
| [] -> false
| h::t -> lor (e == h, member(e,t))
end

type objF('a) = { f : 'a? ; proto : (objF('a))? ..}

let call_f (o:objF('a)) =
  if o is { f : any ..} then o.f
  else if o is { proto : any ..}
  then call_f o.proto
  else ()

type tree('a) = [ ('a\[any*] | tree('a))* ]
let deep_flatten (l : tree('a)) =
  match l with
  | [] -> []
  | (x & :list)::y -> (deep_flatten x) @ (deep_flatten y)
  | x::y -> x::(deep_flatten y)
  end

type expr = ("const", (0..)) | ("add", (expr, expr)) | ("uminus", expr)

let eval (e:expr) =
  match e with
  | (:"add", (e1, e2)) -> (eval e1) + (eval e2)
  | (:"uminus", e) -> 0 - (eval e)
  | (:"const", x) -> x
  end

type obj = { kind:enum }
val obj : obj
let obj = { kind=Book }

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

(* ========= ALTERNATIVES / INFERENCE TRICKS ========= *)

val f1 : int -> int
val f2 : bool -> bool
val f3 : string -> string
val f4 : char -> char
val f5 : () -> ()
val f6 : Nil -> Nil

let test_alt a = [ f1 a | f2 a | f3 a | f4 a | f5 a | f6 a ]
let fall = [ f1 | f2 | f3 | f4 | f5 | f6 ]
let test_noalt a = fall a


(* let typeof x =
  if x is Dbl do return "dbl" end ;
  if x is Lgl do return "lgl" end ;
  if x is Clx do return "clx" end ;
  if x is Chr do return "chr" end ;
  if x is Raw do return "raw" end ;
  if x is Int do return "int" end ;
  if x is Lst do return "lst" end ;
  if x is Null do return "null" end ;
  if x is () do return "unit" end ;
  "object" *)

val typeof :
  (() -> "unit") &
  (~(Null | Lst | Int | Raw | Chr | Clx | Lgl | Dbl | ()) -> "object") &
  (Dbl -> "dbl") & (Lgl -> "lgl") & (Clx -> "clx") & (Chr -> "chr") &
  (Raw -> "raw") & (Int -> "int") & (Lst -> "lst") & (Null -> "null")

val fail : empty -> any
#type_narrowing = false
let typeof_app x =
  let t =
    let y = x in
    suggest y is Dbl or Lgl or Clx or Chr or Raw or Int or Lst or Null or ()
         or ~(Dbl | Lgl | Clx | Chr | Raw | Int | Lst | Null | ()) in
    typeof y
  in
  match t with
  | "int" -> return 0
  | _ -> fail "invalid input"
  end

val typeof1 : (() -> "unit")
val typeof2 : (~(Null | Lst | Int | Raw | Chr | Clx | Lgl | Dbl | ()) -> "object")
val typeof3 : (Dbl -> "dbl")
val typeof4 : (Lgl -> "lgl")
val typeof5 : (Clx -> "clx")
val typeof6 : (Chr -> "chr")
val typeof7 : (Raw -> "raw")
val typeof8 : (Int -> "int")
val typeof9 : (Lst -> "lst")
val typeof10 : (Null -> "null")
val to_bool : (true -> true) & (false -> false)
let typeof_app' x =
  let t = [ typeof1 x | typeof2 x | typeof3 x | typeof4 x |
            typeof5 x | typeof6 x | typeof7 x | typeof8 x |
            typeof9 x | typeof10 x ] in
  if to_bool (if t is "int" then true else false)
  then return 0
  else fail "invalid input"
