#value_restriction = false

type _coerce_('other, 'converted_other, 'converted_self) = {
  _name : ~(enum) ;
  coerce: 'other -> ('converted_other , 'converted_self)
..}

type rational = {
  _name: ~(Rational);
  coerce: 
          (rational -> (rational , rational))
        & (complex  -> ((complex , complex)));
  plus:   (any -> any)
}

and complex = {
  _name: ~(Complex);
  coerce: any -> (complex , complex);
  plus:
     (rational -> complex)
   & (_coerce_(complex, int, bool) -> any)
}

val magic : empty

val a_rational : rational
let a_rational =
  { _name = (Dummy :> ~(Rational)); 
    plus = (fun x -> magic) ; 
    coerce =  (fun x -> magic)
  }
