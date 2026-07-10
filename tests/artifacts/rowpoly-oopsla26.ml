
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

(* ===== Collapsing of unions (aka expansion problem) ===== *)

(* Function foo from Section 5.4.1 *)
val foo: { l1:int ;; `R } -> { l1:int ;; `R }

let test_foo (x: {l1:42;l2:42;l3:42} | {l1:73;l2:73;l3:73}) =
  foo x

(* ===== R language encodings (Appendix A) ===== *)

(* Encoding of arguments: typing lapply (A.1) *)

val mean: { x: [(int|Na)*] ; na_rm: true } | { x: [int*] ; na_rm: false? } -> [int*]
val lapply : { x:['a*] ; y: { x:'a ; y:empty? ;; `r } -> 'b ;; `r } -> ['b*]
let test_lapply = (* Type inferred: [ [int*]* ] *)
  lapply { x=[[1;2;3;4;5;6;7;8;9;10];[1;Na]] ; y=mean ; na_rm=true }

(* Encoding of lists (A.2) *)

val set_b : { b:any? ;; `r } -> 'a -> { b:'a ;; `r }
val set : { ;; `r } -> int -> 'a -> { ;; `r|'a }
val get : { ;; 'a? } -> int -> 'a
val concat: { ;; `A1&(empty?) | `C1&any } -> { ;; `A2&(empty?) | `C2&any } -> { ;; (`A1&`A2) | (`C1|`C2) }
let test_r_lists =
  let mut xs = { a=1 } in         (* xs <- list(a=1) *)
  xs := set_b xs 2 ;              (* xs$b <- 2 *)
  let mut ys = { c=3 } in         (* ys <- list(c=3) *)
  let mut zs = concat xs ys in    (* zs <- append(xs, ys) *)
  let mut n = get zs 2 in         (* n <- zs[[2]] *)
  zs := set zs 1 n ;              (* zs[[1]] <- n *)
  zs (* Type inferred: { b:(1..3) ; a:(1..3) ; c:(1..3) ;; (1..3)? } *)

(* Encoding of classes (A.3) *)

val data_frame : () -> { data_frame:true ;; false }
val group_by : { ;; bool & `c } -> string -> { grouped_df:true ;; bool & `c }
val ungroup : { grouped_df:true ;; bool & `c } -> { grouped_df:false ;; bool & `c }

let test_classes =
    let xs = data_frame () in     (* xs <- data.frame(...) *)
    let ys = group_by xs "id" in  (* ys <- group_by(xs, id) *)
    let zs = ungroup ys in        (* zs <- ungroup(ys) *)
    zs (* Type inferred: { data_frame : true ;; false } *)

val c1 : { c1:true ;; false }
val c1_open : { c1:true ;; bool }
val add_c2 : { ;; bool & `c } -> { c2:true ;; bool & `c }
val need_exactly_c1 : { c1:true ;; false } & 'a -> 'a
val need_c1_c2 : { c1:true ; c2:true ;; bool } & 'a -> 'a
let test_class_ok = need_exactly_c1 c1
let test_class_fail = need_exactly_c1 c1_open
let test_class_ok2 = need_c1_c2 (add_c2 c1_open)
