
val compare : 'a -> 'a -> int
val (<) : 'a -> 'a -> bool
val (=) : 'a -> 'a -> bool
val failwith : string -> empty

#infer_overload = false

let length_aux n l =
  match l with
  | [] -> n
  | _::l -> length_aux (n+1) l
  end

let length l = length_aux 0 l

let hd' l =
  match l with
  | [] -> failwith "hd"
  | x::_ -> x
  end
let hd_safe l =
  match l with
  | x::_ -> x
  end

let tl' l =
  match l with
  | [] -> failwith "tl"
  | _::ll -> ll
  end
let tl_safe l =
  match l with
  | _::ll -> ll
  end

let nth_aux l n =
  match l with
  | [] -> failwith "nth"
  | x::ll -> if n = 0 then x else nth_aux ll (n-1)
  end

let nth l n =
  if n < 0 then failwith "nth"
  else nth_aux l n

let append l1 l2 =
  match l1 with
  | [] -> l2
  | x::l -> x::(append l l2)
  end
let (@) l1 l2 = append l1 l2

let rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x::l -> rev_append l (x::l2)
  end

let flatten l =
  match l with
  | [] -> []
  | x::r -> x @ (flatten r)
  end

let concat l = flatten l

let map f l =
  match l with
  | [] -> []
  | x::ll -> (f x)::(map f ll)
  end

let mapi_aux i f l =
  match l with
  | [] -> []
  | x::ll -> let r = f i x in r::(mapi_aux (i+1) f ll)
  end

let mapi f x = mapi_aux 0 f x

let fold_left f l acc =
  match l with
  | [] -> acc
  | x::ll -> fold_left f ll (f acc x)
  end

let fold_right f acc l =
  match l with
  | [] -> acc
  | x::ll -> f x (fold_right f acc ll)
  end

(* type tree('a) = [ ('a\[any*] | tree('a))* ]
let deep_flatten (l : tree('a)) =
  match l with
  | [] -> []
  | (x & :list)::y -> (deep_flatten x) @ (deep_flatten y)
  | x::y -> x::(deep_flatten y)
  end *)

#infer_overload = true
