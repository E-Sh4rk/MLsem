(* ===== Interfaces ===== *)
#value_restriction = false

type _add_('other, 'return) = {
  _name : any? ;
  plus: 'other -> 'return
..}

type _coerce_('other, 'converted_other, 'converted_self) = {
  _name : ~(enum) ;
  coerce: 'other -> ('converted_other , 'converted_self)
..}

(* ===== Numeric ===== *)

type numeric_('self) = {
  _name: ~(Numeric_);
  coerce: ('self -> ('self , 'self)) & ((any\'self) -> (float , float))
..}
and numeric = numeric_(numeric)

(* ===== Integer ===== *)


and integer_('self) = {
  (* the with construct does not work with mutually recursive types ... commented out *)
  (* numeric_('self) with *) 
  _name: ~(Numeric_ | Integer_);
  coerce: (integer_('self) -> (integer_('self) , integer_('self))) 
        & ((any \ integer_('self)) -> (float , float)) ;
  plus: (integer_('self) -> integer_('self))
      & (_coerce_(integer_('self), ('o & _add_('s, 'r)), 's) -> 'r)
}
and integer = integer_(integer)

(* ===== Float ===== *)

(* Float#coerce drops the self-self branch entirely,
   so it must override numeric_('self)'s coerce. *)

and float_('self) = {
  (* numeric_('self) with *)
  _name: ~(Numeric_ | Float_);
  (* I used any for untyped but a monomorphic type variable '_a may be more appropriate *)
  coerce: any -> (float_('self) , float_('self)); 
  plus: (float_('self)   -> float_('self))
   & (integer -> float_('self))
   & (_coerce_(float_('self), ('o & _add_('s, 'r)), 's) -> 'r)
}
type float = float_(float)


let diverge x = diverge x
let magic = diverge ()
let an_integer = (
  { _name = (Dummy :> ~(Numeric_| Integer_)); 
    plus = (diverge) ; 
    coerce = (diverge)
  } :> integer)

let test0 = an_integer.plus an_integer
