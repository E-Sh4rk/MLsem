#value_restriction = false

type _coerce_('other, 'converted_other) = {
  _name : ~(enum) ;
  coerce: 'other -> 'converted_other
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
   & (_coerce_(complex, int) -> any)
}

val f : any -> empty

val a_rational : rational
let a_rational =
  { _name = Dummy ; 
    plus = f ; 
    coerce =  f
  }
