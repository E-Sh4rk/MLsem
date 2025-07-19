open Variable
open Types.Base
open Types.Tvar
open Types.Gradual

type const =
| Unit | Nil
| EmptyRecord
| Bool of bool
| Int of Z.t
| Float of float
| Char of char
| String of string

val typeof_const : const -> typ

type cf = CfWhile | CfCond
type projection = Pi of int * int | Field of string | Hd | Tl | PiTag of tag
type constructor = Tuple of int | Cons | RecUpd of string | RecDel of string | Tag of tag | Atom of atom
type e =
| Abstract of GTy.t
| Const of const
| Var of Variable.t
| Constructor of constructor * t list
| Lambda of GTy.t * Variable.t * t
| LambdaRec of (GTy.t * Variable.t * t) list
| Ite of t * typ * t * t
| App of t * t
| Projection of projection * t
| Let of (typ list) * Variable.t * t * t
| TypeCast of t * typ
| TypeCoerce of t * GTy.t
| ControlFlow of cf * t * typ * t * t
and t = Eid.t * e

val map : (t -> t) -> t -> t
val fold : (t -> 'a list -> 'a) -> t -> 'a
val fv : t -> VarSet.t
val apply_subst : Subst.t -> t -> t
val substitute : Variable.t -> Variable.t -> t -> t
val coerce : GTy.t -> t -> t

val pp : Format.formatter -> t -> unit
val pp_e : Format.formatter -> e -> unit
val pp_cf : Format.formatter -> cf -> unit
val pp_projection : Format.formatter -> projection -> unit
val pp_constructor : Format.formatter -> constructor -> unit
val pp_const : Format.formatter -> const -> unit
