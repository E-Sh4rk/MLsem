#value_restriction = false

type rational_ = {
  coerce: (A -> (A, A))
        & (rational_ -> (rational_ , rational_))
        & (complex  -> (rational_ , rational_) | (complex , complex))
}
and rational = rational_

and complex_ = {
  coerce: any -> any
}
and complex = complex_

val magic : empty

let a_rational = (
  {
    coerce =  (fun x -> magic)
  } :> rational)
