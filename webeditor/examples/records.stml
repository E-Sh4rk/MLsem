
(* Basics *)

let record_delete x = ((x\l1)\l2)\l3
let record_update x = { x with l1 = 1 ; l2=2 ; l3=3 }

let record_delete_ann (x:{ ;; `a}) = ((x\l1)\l2)\l3
let record_update_ann (x:{ ;; `a}) = { x with l1 = 1 ; l2=2 ; l3=3 }

let record_test =
  let r1 = {l3=33 ; l4=44} in
  let r2 = record_update r1 in
  let r3 = record_delete r2 in
  (r1,r2,r3)

(* Encoding of ellipsis (extra arguments) *)

val extract_ellipsis_param1 : { l1: any? ;; 'a? } -> 'a
val extract_ellipsis_param2 : { l1: any? ; l2: any? ;; 'a? } -> 'a
val extract_ellipsis_param3 : { l1: any? ; l2: any? ; l3: any? ;; 'a? } -> 'a
let extract_ellipsis_test =
  let params = {l1=42 ; l2=73 ; l3=false ; l4=true} in
  extract_ellipsis_param1 params, extract_ellipsis_param2 params, extract_ellipsis_param3 params


val any_element_from_record : { ;; 'a? } -> 'a

let fun2_with_ellipsis r =
  let (a1, a2) = r.l1, r.l2 in
  let ellipsis = (r\l1)\l2 in
  a1,a2,ellipsis,any_element_from_record ellipsis

let fun2_with_ellipsis_test =
  fun2_with_ellipsis { l1=1 ; l2=2 ; l3=3 ; l4=4 ; l5=5 }

(* Fun set-theoretic stuff with the tail *)

val mix: { ;; `A } -> { ;; `B } -> { ;; `A | `B }

let test_mix =
    let r1 = { l1=42 ; l2=33 } in
    let r2 = { l2=true ; l4=false  } in
    mix r1 r2

val merge: { ;; `A1&(empty?) | `C1&any } -> { ;; `A2&(empty?) | `C2&any } -> { ;; (`A1&`A2) | (`C1|`C2) }

let test_merge =
    let r1 = { l1=42 ; l2=33 } in
    let r2 = { l2=true ; l4=false  } in
    merge r1 r2

let test_merge2 x =
    let y = merge x { y=42 ; z=73 } in
    y.x, y.y, y.z

(* ===== R language encodings ===== *)

(* Encoding of arguments: typing lapply *)
val mean: { p1: [(int|Na)*] ; na_rm: true } | { p1: [int*] ; na_rm: false? } -> [int*]
val lapply : { p1:['a*] ; p2: { p1:'a ; p2:empty? ;; `r } -> 'b ;; `r } -> ['b*]
let test_lapply =
  lapply { p1=[[1;2;3;4;5;6;7;8;9;10];[1;Na]] ; p2=mean ; na_rm=true }

(* Encoding of lists *)
val set_b : { b:any? ;; `r } -> 'a -> { b:'a ;; `r }
val set : { ;; `r } -> int -> 'a -> { ;; `r|'a }
val get : { ;; 'a? } -> int -> 'a
val concat: { ;; `A1&(empty?) | `C1&any } -> { ;; `A2&(empty?) | `C2&any } -> { ;; (`A1&`A2) | (`C1|`C2) }
let test_r_lists =
  let mut xs = { a=1 } in
  xs := set_b xs 2 ;
  let mut ys = { c=3 } in
  let mut zs = concat xs ys in
  let mut n = get zs 2 in
  zs := set zs 1 n ;
  zs

(* Encoding of classes *)
val c1 : { c1:true ;; false }
val c1_open : { c1:true ;; bool }
val add_c2 : { ;; bool & `c } -> { c2:true ;; bool & `c }
val need_exactly_c1 : { c1:true ;; false } & 'a -> 'a
val need_c1_c2 : { c1:true ; c2:true ;; bool } & 'a -> 'a
let test_class_ok = need_exactly_c1 c1
let test_class_fail = need_exactly_c1 c1_open
let test_class_ok2 = need_c1_c2 (add_c2 c1_open)
