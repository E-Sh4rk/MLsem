
type rational = {
  l1: 
          (any -> (rational , rational))
        & (any -> (complex , complex));
  l2:   (any -> any)
}

and complex = {
  l1: any -> any ;
  l2:
     (rational -> complex)
   & ({ l1: int -> int ..} -> any)
}

val f : any -> empty

let a_rational =
  ({ 
    l1 = f ;
    l2 = f
  } :> rational)
