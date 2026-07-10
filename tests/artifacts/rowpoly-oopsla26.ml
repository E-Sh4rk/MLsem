
(* ===== Basics ===== *)

let record_delete x = ((x\l1)\l2)\l3
let record_update x = { x with l1 = 1 ; l2=2 ; l3=3 }

let record_delete_ann (x:{ ;; `a}) = ((x\l1)\l2)\l3
let record_update_ann (x:{ ;; `a}) = { x with l1 = 1 ; l2=2 ; l3=3 }

let record_test =
  let r1 = {l3=33 ; l4=44} in
  let r2 = record_update r1 in
  let r3 = record_delete r2 in
  (r1,r2,r3)

(* ===== Per-field semantics of the tail ===== *)

(* Function mix from Section 5.4.2 *)
val mix: { ;; `A } -> { ;; `B } -> { ;; `A | `B }

let test_mix =
    let r1 = { l1=42 ; l2=33 } in
    let r2 = { l2=true ; l4=false  } in
    mix r1 r2

(* Variant of the mix function, where fields present in only one record are taken from that record *)
val merge: { ;; `A1&(empty?) | `C1&any } -> { ;; `A2&(empty?) | `C2&any } -> { ;; (`A1&`A2) | (`C1|`C2) }

let test_merge =
    let r1 = { l1=42 ; l2=33 } in
    let r2 = { l2=true ; l4=false  } in
    merge r1 r2

let test_merge2 x =
    let y = merge x { y=42 ; z=73 } in
    y.x, y.y, y.z

(* TODO: foo from Section 5.4.1 *)

(* ===== R language encodings ===== *)

(* Encoding of arguments: typing lapply *)
val mean: { x: [(int|Na)*] ; na_rm: true } | { x: [int*] ; na_rm: false? } -> [int*]
val lapply : { x:['a*] ; y: { x:'a ; y:empty? ;; `r } -> 'b ;; `r } -> ['b*]
let test_lapply =
  lapply { x=[[1;2;3;4;5;6;7;8;9;10];[1;Na]] ; y=mean ; na_rm=true }

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
val data_frame : () -> { data_frame:true ;; false }
val group_by : { ;; bool & `c } -> string -> { grouped_df:true ;; bool & `c }
val ungroup : { grouped_df:true ;; bool & `c } -> { grouped_df:false ;; bool & `c }

let test_classes =
    let xs = data_frame () in
    let ys = group_by xs "id" in
    let zs = ungroup ys in
    zs

val c1 : { c1:true ;; false }
val c1_open : { c1:true ;; bool }
val add_c2 : { ;; bool & `c } -> { c2:true ;; bool & `c }
val need_exactly_c1 : { c1:true ;; false } & 'a -> 'a
val need_c1_c2 : { c1:true ; c2:true ;; bool } & 'a -> 'a
let test_class_ok = need_exactly_c1 c1
let test_class_fail = need_exactly_c1 c1_open
let test_class_ok2 = need_c1_c2 (add_c2 c1_open)
