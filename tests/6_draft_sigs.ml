val (+) : (int, int) -> int
val (-) : (int, int) -> int
val ( * ) : (int, int) -> int
val (/) : (int, int) -> int
val (%) : (int, int) -> int
val succ : int -> int


val foo : int -> _
let foo x = x

val bar : _ -> int
let bar x = x

val foobar : _ -> _
let foobar x = x


val overload : _ -> any
let overload x =
    match x with
    | :int -> true
    | :bool -> 42
    end

val curry : int -> _
let curry x y = x + y

val two : int -> int -> _
val two : 1 -> 2 -> bool
let two x y = if x is 1 then if y is 2 then true else y else y

val even2 : int -> _
val odd2 : _ -> bool
let even2 n = if n is 0 then true else odd2 (n-1)
and odd2 n = if n is 0 then false else even2 (n-1)


val len2 : [] -> _
val len2 : [any+] -> _
let len2 l = match l with [] -> 0 | _::xs -> 1 + len2 xs end
