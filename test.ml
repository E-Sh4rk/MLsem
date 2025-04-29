(* ========= TESTS OBJECTS ======== *)

type objF('a) = { f :? 'a ; proto :? objF('a) ..}

let rec call_f (o:objF('a)) =
  if o is { f : any ..} then o.f
  else if o is { proto : any ..}
  then call_f o.proto
  else ()

(* ========= ABSTRACT TYPES ======== *)

abstract type cav(-'a)
abstract type cov(+'a)
abstract type inv('a)

let test_neg1 = <cav & ~cav(int)>
let test_neg2 = < ~cav(int)>
let test_neg3 = <cov & ~cov(int)>
let test_neg4 = < ~cov(int)>
let test_neg5 = <inv & ~inv(int)>
let test_neg6 = < ~inv(int)>

let cav = <cav(A) & cav(~A)>
let cav = <cav(A|B) & cav(B|C)>
let cav = <cav(A|B) & cav(B|C) & ~cav(any)>

let cov = <cov(A) & cov(~A)>
let cov = <cov(A|B) & cov(B|C)>
let cov = <cov(A|B) & cov(B|C) & ~cov(empty)>

let inv = <inv(A) & inv(B) & inv(A|B)>

abstract type ref('a)
let ref = <'a -> ref('a)>
let set = <ref('a) -> 'a -> ref('a)> (* We dont have value restriction yet, so functions should not have effects *)
let get = <ref('a) -> 'a>

let test_ref = ref 42 (* No value restriction yet :( *)
let test_ref x =
  let y = ref x in
  let y = set y 42 in
  get y

let is_ref x = if x is ref then true else false
let is_not_ref x = if x is ~ref then true else false
(* let incalid_typecase x = if x is ref(int) then true else false *)

abstract type map(-'k, +'v)
let mk_map = <() -> map('a, 'b)>
let add_map = <map('a, 'b) -> 'a -> 'b -> map('a, 'b)>
let get_map = <map('a, 'b) -> 'a -> 'b>

let test_map x =
  let map = mk_map () in
  let map = add_map map x 42 in
  let map = add_map map "key" 0 in
  (map, get_map map false)

(* ========= TAGGED VALUES ======== *)

let proj_a (A(v)) = v
let proj_ab x =
  match x with
  | A(v) -> v
  | B(v) -> v
  end

type clist('a) = Nil | Cons('a, clist('a))
let rec map_clist f (lst:clist('a)) =
  match lst with
  | Cons(v,tail) -> Cons(f v, map_clist f tail)
  | Nil -> Nil
  end

(* ================================= *)
let succ = <int->int>

let aliasing (x : any -> any) = 
  let y = x in if x y is int then (y x) + 1 else 42

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
  if land (lor x (lnot x)) (lor (lnot y) y) then true else false

(* ============== RECURSIVE ============= *)

(* The type deduced for fixpoint can be read as follows
   forall('c <: 'a -> 'b)('d <:'c). ('c -> 'd) -> 'd 
*)
let fixpoint = fun f ->
  let delta = fun x ->
     f ( fun  v -> ( x x v ))
   in delta delta

let fact fact n =
  if n is 0 then 1 else (fact (n-1))*n

let fact = fixpoint fact

let length length lst =
  if lst is [] then 0 else succ (length (tl lst))

let length = fixpoint length

let map map f lst =
  if lst is [] then []
  else (f (hd lst))::(map f (tl lst))

let map = fixpoint map

(*************************************************
*          Tobin-Hochstadt & Felleisen           *
*     exampleX = EXPLICITLY ANNOTATED VERSIONS   *
*     implictX = IMPLICITLY ANNOTATED VERSIONS   *
*                                                *
**************************************************)

(*
 Interesting points:
  - example2: does not need the annotation, while TH&F yes
  - example6: not typable with the annotation int|string
    (as expected), but if we remove annotations becomes typable.
    That is our system finds the right constraints to make the
    expression typable
  - in examples 10 11 12 we do not have to assume that p is
    a product the system deduces it alone
  - same for the example 14. We do not have to assume that
    the parameter input is int|string and extra is a pair. The system
    finds it alone and it works for user defined "and"
    (currified or not)
*)

(* prelude *)

let and_ = fun x -> fun y ->
     if x is true then if y is true then y else false else false
let fst2 = <('a, any) -> 'a>
let snd2 = <(any, 'a) -> 'a>
let and2_ = fun x ->
  if fst2 x is true then if snd2 x is true then fst2 x else false else false
let and2_ = fun x ->
     if fst x is true then if snd x is true then fst x else false else false

let not_ = fun x -> if x is true then false else true

let or_ =  fun x -> fun y -> not_ (and_ (not_ x) (not_ y))

let is_string = fun x ->
     if x is string then true else false

let is_int = fun x ->
     if x is int then true else false

let strlen = <(string) -> int>

let add = <int -> int -> int>

let add1 = <int -> int>

let f = <(int | string) -> int>

let g = <(int, int) -> int>

(* Examples Tobin-Hochstadt & Felleisen *)

let example1 = fun (x:any) ->
  if x is int then add1 x else 0

let implict1 = fun x ->
  if x is int then add1 x else 0


let example2 = fun (x:string|int) ->
  if x is int then add1 x else strlen x

let implict2 = fun x ->
  if x is int then add1 x else strlen x


let example3 = fun (x: any) ->
  if x is (any \ false) then (x,x) else false

let implict3 = fun x ->
  if x is (any \ false) then (x,x) else false


let example4 = fun (x : any) ->
  if or_ (is_int x) (is_string x) is true then x else 'A'

let implict4 = fun x ->
  if or_ (is_int x) (is_string x) is true then x else 'A'


let example5 = fun (x : any) -> fun (y : any) ->
  if and_ (is_int x) (is_string y) is true then
   add x (strlen y) else 0

let implict5 = fun x -> fun y ->
  if and_ (is_int x) (is_string y) is true then
   add x (strlen y) else 0


let example6 = fun (x : int|string) -> fun (y : any) ->
  if and_ (is_int x) (is_string y) is true then
   add  x (strlen y) else strlen x

let implict6 = fun x -> fun y ->
  if and_ (is_int x) (is_string y) is true then
   add  x (strlen y) else strlen x


let example7 = fun (x : any) -> fun (y : any) ->
  if  (if (is_int x) is true then (is_string y) else false) is true then
   add x (strlen y) else 0

let implict7 = fun x -> fun y ->
  if  (if (is_int x) is true then (is_string y) else false) is true then
   add x (strlen y) else 0


let example8 = fun (x : any) ->
  if or_ (is_int x) (is_string x) is true then true else false

let implict8 = fun x ->
  if or_ (is_int x) (is_string x) is true then true else false


let example9 = fun (x : any) ->
  if
   (if is_int x is true then is_int x else is_string x)
   is true then  f x else 0

let implict9 = fun x  ->
  if
   (if is_int x is true then is_int x else is_string x)
   is true then  f x else 0


let example10 = fun (p : (any,any)) ->
  if is_int (fst p) is true then add1 (fst p) else 7

let implict10 = fun p ->
  if is_int (fst p) is true then add1 (fst p) else 7


let example11 = fun (p : (any, any)) ->
  if and_ (is_int (fst p)) (is_int (snd p)) is true then g p else No

let implict11 = fun p ->
  if and_ (is_int (fst p)) (is_int (snd p)) is true then g p else No

let example12 = fun (p : (any, any)) ->
  if is_int (fst p) is true then true else false

let implict12 = fun p ->
  if is_int (fst p) is true then true else false


let example13 =
 fun (x : any) ->
   fun (y : any) ->
    if and_ (is_int x) (is_string y) is true then 1
    else if is_int x is true then 2
    else 3

let implict13 =
 fun x ->
   fun y ->
    if and_ (is_int x) (is_string y) is true then 1
    else if is_int x is true then 2
    else 3


(* uncurried "and" *)
let example14 = fun (input : int|string) ->
fun (extra : (any, any)) ->
 if and2_(is_int input , is_int(fst extra)) is true then
     add input (fst extra)
 else if is_int(fst extra) is true then
     add (strlen input) (fst extra)
 else 0

let implct14a = fun (input : int|string) ->
fun extra ->
 if and2_(is_int input , is_int(fst extra)) is true then
     add input (fst extra)
 else if is_int(fst extra) is true then
     add (strlen input) (fst extra)
 else 0

let implct14b = fun input ->
fun extra ->
 if and2_(is_int input , is_int(fst extra)) is true then
     add input (fst extra)
 else if is_int(fst extra) is true then
     add (strlen input) (fst extra)
 else 0

(* curried "and" *)
let curried14 = fun (input : int|string) ->
fun (extra : (any, any)) ->
 if and_ (is_int input) (is_int(fst extra)) is true then
     add input (fst extra)
 else if is_int (fst extra) is true then
     add (strlen input) (fst extra)
 else 0

let currid14a = fun (input : int|string) ->
fun extra ->
 if and_ (is_int input) (is_int(fst extra)) is true then
     add input (fst extra)
 else if is_int (fst extra) is true then
     add (strlen input) (fst extra)
 else 0

let currid14b = fun input ->
fun extra ->
 if and_ (is_int input) (is_int(fst extra)) is true then
     add input (fst extra)
 else if is_int (fst extra) is true then
     add (strlen input) (fst extra)
else 0

(* === Filter map === *)

let rec filtermap ((f, l) : (any, [])) =
  match l with
  | [] -> []
  | (x::xs) ->
      match f x with
      | :false -> filtermap (f, xs)
      | :true -> x::(filtermap (f, xs))
      | (:true, y) -> y::(filtermap (f, xs))
    end
  end

let rec filtermap ((f, l) : ('a -> false | (true, 'b), ['a*])) =
  match l with
  | [] -> []
  | (x::xs) ->
      match f x with
      | :false -> filtermap (f, xs)
      | :true -> x::(filtermap (f, xs))
      | (:true, y) -> y::(filtermap (f, xs))
    end
  end

let rec filtermap ((f, l) : ('a -> bool | (true, 'b), ['a*])) =
  match l with
  | [] -> []
  | (x::xs) ->
      match f x with
      | :false -> filtermap (f, xs)
      | :true -> x::(filtermap (f, xs))
      | (:true, y) -> y::(filtermap (f, xs))
    end
  end

(*******************************
 *                             *
 *  Examples for polymorphism  *
 *                             *
 *******************************)

type falsy = false | "" | 0
type truthy = ~falsy

let and_js = fun x -> fun y ->
  if x is falsy then x else y

let not_js = fun x -> if x is falsy then 1 else 0

let or_js = fun x -> fun y ->
  if x is truthy then x else y

let identity_js = fun x -> or_js x x

let and_pair = fun x -> fun y ->
  if x is falsy then x else (y, succ x)

let test = fun x ->
  if fst x is falsy then (fst x) + (snd x) else succ (fst x)

let rec concat (x:['a*]) (y:['b*]) =
   if x is [] then y else (hd x)::(concat (tl x) y)

let concat : ['a*] -> ['b*] -> ['a* 'b*] = concat

let rec flatten_ocaml (x:[['a*]*])  =
  if x is [] then [] else concat (hd x) (flatten_ocaml (tl x))

let reverse_stub reverse l =
  if l is [] then [] else concat (reverse (tl l)) [hd l]

let reverse = fixpoint reverse_stub

(*
let rev_tl_stub rev_tl l acc  =
     if l is [] then acc else rev_tl (snd l) (fst l, acc)

let rev_tl l = (fixpoint rev_tl_stub) l []

let foldr_stub foldr f l acc =
   if l is [] then acc else f (fst l) (foldr f (snd l) acc)

let foldr = fixpoint foldr_stub

let foldr_ann : ('a -> 'b -> 'b ) -> [ 'a* ] -> 'b -> 'b = foldr

(* MANY VARIANTS OF FILTER *)

let filter_stub filter (f: ('a->true) & ('b -> ~true)) (l:[('a|'b)*]) =
   if l is [] then [] else
   if l is [any+] then
       if f(fst(l)) is true then (fst(l),filter f (snd(l))) else filter f (snd(l))
   else 42(3)

let filter = fixpoint filter_stub

let filter2_stub
  (filter : ((('a & 'b) -> any) & (('a\'b) -> ~true)) -> [ 'a* ] -> [ ('a&'b)* ] )
  (f : (('a & 'b) -> any) & (('a\'b) -> ~true))
  (l : [ ('a)*  ] )  =
  (* filter f l = *)
  if l is [] then []
  else
    if f(fst(l)) is true then (fst(l),filter f (snd(l))) else filter f (snd(l))

let filter2 :  ((('a & 'b) -> any) & (('a\'b) -> ~true)) -> [ 'a* ] -> [ ('a&'b)* ] =
      fixpoint filter2_stub

let x = <int -> bool>

let filter2_partial_app = filter2 x

(*
    Here a better version with head and tail:
    it yields exactly the same type as the version above
*)

let filter3_stub
  (filter : ((('a & 'b) -> true) & (('a\'b) -> ~true)) -> [ 'a* ] -> [ ('a&'b)* ] )
  (f : (('a & 'b) -> true) & (('a\'b) -> ~true))
  (l : [ ('a)*  ] )  =
   if l is [] then [] else
       let h = fst(l) in
       let t = snd(l) in
       if f h is true then (h ,filter f t) else filter f t

let filter3 :  ((('a & 'b) -> true) & (('a\'b) -> ~true)) -> [ 'a* ] -> [ ('a&'b)* ] =
      fixpoint filter3_stub

let filter4_stub
  (filter : ((('a) -> true) & (('b) -> ~true)) -> [ ('a|'b)* ] -> [ ('a)* ] )
  (f : (('a) -> true) & (('b) -> ~true))
  (l : [ ('a|'b)* ] )  =
   if l is [] then [] else
       let h = fst(l) in
       let t = snd(l) in
       if f h is true then (h ,filter f t) else filter f t

let filter4 : ((('a) -> true) & (('b) -> ~true)) -> [ ('a|'b)* ] -> [ ('a)* ] =
      fixpoint filter4_stub

let xi = <(int -> true) & (bool -> false)>

let filter3_test = filter3 xi [1;3;true;42]

let filter4_test = filter4 xi (1, (3, (true,(42,[]))))

(* cross typing on the two versions *)

let filter4_as_3 : ((('a & 'b) -> true) & (('a\'b) -> ~true)) -> [ 'a* ] -> [ ('a&'b)* ] =
      fixpoint filter4_stub

let filter3_as_4 : ((('a) -> true) & (('b) -> ~true)) -> [ ('a|'b)* ] -> [ ('a)* ]  =
      fixpoint filter3_stub

let filter_classic_stub
  (filter : (('a) -> bool) -> [ ('a)* ] -> [ ('a)* ] ) ( f : 'a -> bool) (l : [ ('a)* ] ) =
  (* filter f l = *)
  if l is [] then []
  else
    if f(fst(l)) is true then (fst(l),filter f (snd(l))) else filter f (snd(l))

let filter_classic = fixpoint filter_classic_stub

(* A version where the predicate function must cover any *)

let filter_total_stub
  (filter : (('a -> true) & ((~('a)) -> ~true)) -> [ any* ] -> [ ('a)* ] )
  ( f : (('a -> true) & ((~('a)) -> ~true))) (l : [ any* ] )  =
   if l is [] then [] else
   if f(fst(l)) is true then (fst(l),filter f (snd(l))) else filter f (snd(l))

let filter_total : (('a -> true) & ((~'a) -> ~true)) -> [any*] -> [ ('a)* ] = fixpoint filter_total_stub

(* DEEP FLATTEN FUNCTION *)

let flatten_noannot_stub flatten x =
  if x is [] then [] else
  if x is [any*] then concat (flatten (fst x)) (flatten (snd x))
  else (x,[])

(* let flatten_noannot = fixpoint flatten_noannot_stub *)

type Tree 'a = ('a \ [any*]) | [(Tree 'a)*]

let flatten_stub flatten (x : Tree 'a) =
  if x is [] then [] else
  if x is [any*] then concat (flatten (fst x)) (flatten (snd x))
  else (x,[])

let flatten = fixpoint flatten_stub

let flatten_ann : (Tree 'a -> ['a*]) = flatten 

let test_flatten = flatten ((1,(true,[])),(((42,(false,[])),0),"ok"))

(* MISCELLANEOUS *)

type TRUE 'a 'b  =  'a -> 'b -> 'a
type FALSE 'a 'b  =  'a -> 'b -> 'b

let ifthenelse (b : TRUE 'a 'b; FALSE 'a 'b )  x y = b x y

let check :    (TRUE 'c 'd -> 'c -> 'd -> 'c) & (FALSE 'c 'd -> 'c -> 'd -> 'd) = ifthenelse

(* Parametric types examples *)

type Tree' 'a = ('a, [(Tree' 'a)*])
let a = <Tree' int>

type Rec 'a = Rec 'a -> 'a
let b = <Rec 'b>

(* Pattern matching *)

let test_patterns (a,_) = a

let test2_patterns x =
  match x with (a,_)&(_,b) -> (a,b) end

let test3_patterns x y =
  let pack x y = (x,y) in
  let (y,x) = pack x y in
  pack x y

let test3_patterns_ann x y =
  let pack (x:'a;'b) (y:'a;'b) = (x,y) in
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
  | :true, :true -> true
  | :any -> false
  end

let fact_pat_stub fact n =
  match n with
  | :0 -> 1
  | n -> (fact (n-1))*n
  end

let fact_pat = fixpoint fact_pat_stub

let length_pat_stub length lst =
  match lst with
  | [] -> 0
  | (_, tl & :list) -> succ (length tl)
  end

let length_pat = fixpoint length_pat_stub

let map_pat_stub map f lst =
  match lst with
  | [] -> []
  | (hd, tl) & :list -> (f hd, map f tl)
  end

let map_pat = fixpoint map_pat_stub

(* Recursive functions and partial user type annotations *)

let rec map_noannot f lst =
  match lst with
  | [] -> []
  | (e,lst) & :list -> ((f e), map_noannot f lst)
  end

let rec map f (lst:['a*]) =
  match lst with
  | [] -> []
  | (e,lst) & :list -> ((f e), map f lst)
  end

(* let rec filter_noannot f l =
  if l is [] then []
  else
    if f(fst(l)) is true
    then (fst(l),filter f (snd(l)))
    else filter f (snd(l)) *)

let rec filter (f: ('a->any) & ('b -> ~true)) (l:[('a|'b)*]) =
  match l with
  | :[] -> []
  | (e,l) ->
    if f e is true
    then (e, filter f l)
    else filter f l
  end
    
(* let rec flatten_noannot x =
  if x is [] then [] else
  if x is [any*] then concat (flatten (fst x)) (flatten (snd x))
  else (x,[]) *)

let rec flatten (x : Tree('a)) =    
  if x is [] then [] else
  if x is [any*] then concat (flatten (fst x)) (flatten (snd x))
  else (x,[])

let rec mapi_aux i f l =
  match l with
  :[] -> []
  | (x, ll) -> let r = f i x in (r, mapi_aux (i+1) f ll)
end

let mapi f l = mapi_aux 0 f l
  
let rec eval e =
  match e with
  | (:"add", (e1, e2)) -> (eval e1) + (eval e2)
  | (:"uminus", e) -> 0 - (eval e)
  | (:"const", x) -> x
  end

type Expr = ("const", (0--)) | ("add", (Expr, Expr)) | ("uminus", Expr)

let rec eval_ann (e:Expr) =
  match e with
  | (:"add", (e1, e2)) -> (eval_ann e1) + (eval_ann e2)
  | (:"uminus", e) -> 0 - (eval_ann e)
  | (:"const", x) -> x
  end

(* ===== EXAMPLES FROM THE PAPER ===== *)

let toBoolean x =
    if x is truthy then true else false

let lOr (x,y) =
    if toBoolean x then x else y

let id x =
    lOr (x,x)

let fixpoint = fun f ->
  let delta = fun x ->
      f ( fun  v -> ( x x v ))
  in delta delta

let map_stub map f lst =
  if lst is [] then []
  else (f (fst lst), map f (snd lst))

let map = fixpoint map_stub

let filter_stub filter (f: ('a->any) & ('b -> ~true)) (l:[('a|'b)*]) =
  if l is [] then []
  else if f(fst(l)) is true
  then (fst(l), filter f (snd(l)))
  else filter f (snd(l))

let filter = fixpoint filter_stub

let rec (concat : ['a*] -> ['b*] -> ['a* ; 'b*]) x y =
  match x with
  | [] -> y
  | (h, t) -> (h, concat t y)
  end

let rec flatten x = match x with
 | [] -> []
 | (h, t) & :list -> concat (flatten h) (flatten t)
 | _ -> [x]
end

let rec filter f (l:['a*]) =
  match l with
  | [] -> []
  | (h,t) & :list -> if f h
             then (h, filter f t)
             else filter f t
  end

let rec fold_right f acc l =
  match l with
  :[] -> acc
  | (x, ll) -> f x (fold_right f acc ll)
end

(* UNCOMPLETENESS & ANNOTATIONS *)

let id x = x

let test_expansion_noannot =
    let f = (fun x -> (x 123, x true)) in
    f id

let test_expansion =
  let f = (fun x -> (x 123, x true)) in
  f (id :> (123 -> 123) ; (true -> true))

let f = < ('a -> 'a) -> ('a -> 'a) >
let x = < (int -> int) | (bool -> bool) >

let test_expansion2_noannot = f x

let test_expansion2 =
  (f :> ((int -> int) -> (int -> int)) ;
        ((bool -> bool) -> (bool -> bool))) x

let bool = <() -> bool>
let neg = <(true -> false) & (false -> true)>
let lor = <(true -> bool -> true)
  & (bool -> true -> true) & (false -> false -> false)>

let test_split_noannot =
  let b = bool () in
  lor (neg b) b

let test_split =
  let b = (bool () : true ; false) in
  lor (neg b) b

(* Cons syntax *)

let hd2 x =
  match x with
  | _::b::_ -> b
  end

let cons2 x y z = x::y::z

let test_cons = hd2 (cons2 'a' 'b' ['c';'d'])

let rec first_leaf (x : T | () where T = [T+] | int) =
  match x with
  | () -> []
  | b::_ -> first_leaf b
  | i -> i
  end

(* BAL *)

let (>=) = <Int -> Int -> Bool>
let (>) = <Int -> Int -> Bool>
let invalid_arg = <String -> Empty>

atoms key

type T 'a =
  Nil | (T 'a, Key, 'a, T 'a, Int)

let height x =
  match x with
  | :Nil -> 0
  | (_,_,_,_,h) -> h
  end

let create l x d r =
  let hl = height l in
  let hr = height r in
  (l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal (l:T 'a) (x: Key) (d:'a) (r:T 'a) =
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

let bal : T 'a -> Key -> 'a -> T 'a -> T 'a = bal