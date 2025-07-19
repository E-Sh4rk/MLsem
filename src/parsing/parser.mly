%{ (* Emacs, use -*- tuareg -*- to open this file. *)

  open System.Ast
  open Ast
  open Types.Additions
  open Types.Base
  open Sstt.Extensions

  let annot sp ep e =
    (Ast.new_annot (Position.lex_join sp ep), e)

  let tmp_var = "__encoding"
  let abstraction startpos endpos lst t =
    let step acc (da, pat) =
      match pat with
      | PatVar v -> annot startpos endpos (Lambda (v, da, acc))
      | pat ->
        let test = annot startpos endpos (Var tmp_var) in
        let body = annot startpos endpos (PatMatch (test, [(pat, acc)])) in
        annot startpos endpos (Lambda (tmp_var, da, body))
    in
    List.rev lst |> List.fold_left step t

  let let_pattern startpos endpos pat d t =
    match pat with
    | PatVar v -> annot startpos endpos (Let (v, d, t))
    | pat -> annot startpos endpos (PatMatch (d, [(pat, t)]))

  let double_app startpos endpos f a b =
    let app1 = annot startpos endpos (App (f, a)) in
    annot startpos endpos (App (app1, b))

  let rec list_of_elts startpos endpos = function
    | [] -> annot startpos endpos (Const Nil)
    | x::xs ->
    let left = x in let right = list_of_elts startpos endpos xs in
    annot startpos endpos (Cons (left,right))

  let rec record_update startpos endpos base = function
    | [] -> base
    | (label,e)::fields ->
      let base = annot startpos endpos (RecordUpdate (base, label, Some e)) in
      record_update startpos endpos base fields

  let rec list_of_pats = function
    | [] -> (PatType (TBase TNil))
    | x::xs ->
      let left = x in let right = list_of_pats xs in
      PatCons (left,right)

  let builtin_type_or_custom str =
    match str with
    | "empty" -> TBase TEmpty
    | "any" -> TBase TAny
    | "tuple" -> TBase TTupleAny
    | "arrow" -> TBase TArrowAny
    | "record" -> TBase TRecordAny
    | "atom" -> TBase TAtomAny
    | "tag" -> TBase TTagAny
    | "int" -> TBase (TInt (None, None))
    | "char" -> TBase TChar
    | "float" -> TBase TFloat
    | "string" -> TBase TString
    | "list" -> TBase TList
    | "bool" -> TBase TBool
    | str ->
      let regexp = Str.regexp {|^tuple\([0-9]*\)$|} in
      if Str.string_match regexp str 0 then
        let nb = Str.matched_group 1 str in
        TBase (TTupleN (int_of_string nb))
      else
        TCustom str
%}

%token EOF
%token FUN VAL LET IN FST SND HD TL HASHTAG SUGGEST
%token IF IS THEN ELSE WHILE DO BEGIN
%token LPAREN RPAREN IRPAREN EQUAL COMMA CONS COLON COLON_OPT COERCE COERCE_STATIC
%token INTERROGATION_MARK EXCLAMATION_MARK
%token ARROW AND OR NEG DIFF
%token TIMES PLUS MINUS DIV
%token LBRACE RBRACE DOUBLEPOINT MATCH WITH END POINT LT GT
%token AND_KW OR_KW
%token TYPE WHERE ABSTRACT
%token LBRACKET RBRACKET SEMICOLON
%token<string> ID IID CID PCID
%token<string> TVAR TVAR_WEAK
%token<float> LFLOAT
%token<Z.t> LINT
%token<bool> LBOOL
%token<char> LCHAR
%token<string> LSTRING
%token<string> INFIX PREFIX INDEXED OPID

%type<Ast.parser_expr> term
%start<Ast.parser_expr> unique_term
%start<Ast.parser_program> program

%right ARROW
%left OR
%left AND
%left DIFF
%right CONS
%nonassoc NEG

%%

program: e=element* EOF { e }

unique_term: t=term EOF { t }

%inline tl_let:
| id=generalized_identifier ais=parameter* EQUAL t=term
{
  let t = abstraction $startpos $endpos ais t in
  (id, t)
}

element:
| LET ds=separated_nonempty_list(AND_KW, tl_let) { annot $symbolstartpos $endpos (Definitions ds) }
| VAL id=generalized_identifier COLON ty=typ { annot $symbolstartpos $endpos (SigDef (id, ty)) }
| TYPE ts=separated_nonempty_list(AND_KW, param_type_def) { annot $symbolstartpos $endpos (Types ts) }
| ABSTRACT TYPE name=ID params=abs_params { annot $symbolstartpos $endpos (AbsType (name, params)) }
| HASHTAG cmd=ID EQUAL v=literal { annot $symbolstartpos $endpos (Command (cmd, v)) }

%inline abs_params:
  { [] }
| LPAREN vs=separated_nonempty_list(COMMA, variance) RPAREN { vs }

variance:
  TVAR { Inv }
| PLUS TVAR { Cov }
| MINUS TVAR { Cav }

(* ===== TERMS ===== *)

%inline optional_test_type:
  { TBase TTrue }
| IS t=typ { t }

term:
  t=simple_term { t }
| FUN ais=parameter+ ARROW t = terms { abstraction $startpos $endpos ais t }
| LET id=generalized_identifier ais=parameter* EQUAL td=term IN t=terms
  {
    let td = abstraction $startpos $endpos ais td in
    annot $startpos $endpos (Let (id, td, t))
  }
| LET p=ppattern EQUAL td=term IN t=terms { let_pattern $startpos $endpos p td t }
| SUGGEST id=generalized_identifier IS tys=separated_nonempty_list(OR_KW, typ) IN t=terms
{ annot $startpos $endpos (Suggest (id, tys, t)) }
| IF t=term ott=optional_test_type THEN t1=term ELSE t2=term { annot $startpos $endpos (Ite (t,ott,t1,t2)) }

terms:
  a=term { a }
| t1=simple_term SEMICOLON t2=terms { annot $startpos $endpos (Seq (t1,t2)) }

simple_term: (* Cannot end with a semi-colon *)
  a=simple_term2 { a }
| IF t=term ott=optional_test_type DO t1=terms END { annot $startpos $endpos (Cond (t,ott,t1,None)) }
| IF t=term ott=optional_test_type DO t1=terms ELSE t2=terms END { annot $startpos $endpos (Cond (t,ott,t1,Some t2)) }
| WHILE t=term ott=optional_test_type DO t1=terms END { annot $startpos $endpos (While (t,ott,t1)) }
| MATCH t=term WITH pats=patterns END { annot $startpos $endpos (PatMatch (t,pats)) }
| hd=simple_term2 COMMA tl=separated_nonempty_list(COMMA, simple_term2) { annot $startpos $endpos (Tuple (hd::tl)) }

simple_term2:
  a=simple_term3 { a }
| lhs=simple_term3 CONS rhs=simple_term2 { annot $startpos $endpos (Cons (lhs, rhs)) }

simple_term3:
  a=simple_term4 { a }
| a=simple_term3 b=simple_term4 { annot $startpos $endpos (App (a, b)) }
| p=proj a=simple_term4 { annot $startpos $endpos (Projection (p, a)) }
| a=simple_term4 s=infix_term b=simple_term4 { double_app $startpos $endpos s a b }
| LT t=typ GT { annot $startpos $endpos (Abstract t) }
| t1=indexed i=INDEXED t2=simple_term4
{
  let f = annot $startpos $endpos (Var ("["^i)) in
  let app = annot $startpos $endpos (App (f, t1)) in
  annot $startpos $endpos (App (app, t2))
}

simple_term4:
  a=atomic_term { a }
| a=atomic_term POINT id=ID { annot $startpos $endpos (Projection (Field id, a)) }
| a=atomic_term DIFF id=ID { annot $startpos $endpos (RecordUpdate (a,id,None)) }
| p=prefix_term a=simple_term4 { annot $startpos $endpos (App (p, a)) }

%inline indexed:
| x=IID t=term { annot $startpos $endpos (Tuple [ annot $startpos $endpos (Var x) ; t ]) }
| LPAREN t1=terms IRPAREN t2=term { annot $startpos $endpos (Tuple [ t1 ; t2 ]) }

proj:
| FST { Pi(2,0) } | SND { Pi(2,1) } | HD { Hd } | TL { Tl }

infix_term:
  x=infix { annot $startpos $endpos (Var x) }

prefix_term:
  x=prefix { annot $startpos $endpos (Var x) }

atomic_term:
  x=generalized_identifier { annot $startpos $endpos (Var x) }
| t=indexed RBRACKET
{
  let f = annot $startpos $endpos (Var "[]") in
  annot $startpos $endpos (App (f, t))
}
| c=CID { annot $startpos $endpos (Atom c) }
| t=PCID a=term RPAREN { annot $startpos $endpos (Tag (t,a)) }
| t=PCID RPAREN { annot $startpos $endpos (Tag (t,annot $startpos $endpos (Const Unit))) }
| l=literal { annot $startpos $endpos (Const l) }
| LPAREN RPAREN { annot $startpos $endpos (Const Unit) }
| LPAREN t=terms RPAREN { t }
| BEGIN t=terms END { t }
| LPAREN t=term IS ty=typ RPAREN {
  let annot = annot $startpos $endpos in
  annot (Ite (t,ty,annot (Const (Bool true)),annot (Const (Bool false))))
  }
| LPAREN t=term COLON ty=typ RPAREN { annot $startpos $endpos (TypeCast (t,ty)) }
| LPAREN t=term COERCE ty=typ RPAREN { annot $startpos $endpos (TypeCoerce (t,ty,false)) }
| LPAREN t=term COERCE_STATIC ty=typ RPAREN { annot $startpos $endpos (TypeCoerce (t,ty,true)) }
| LBRACE obr=optional_base_record fs=separated_list(SEMICOLON, field_term) RBRACE
{ record_update $startpos $endpos obr fs }
| LBRACKET lst=separated_list(SEMICOLON, simple_term) RBRACKET
{ list_of_elts $startpos $endpos lst }

%inline optional_base_record:
  { annot $startpos $endpos (Const EmptyRecord) }
| a=atomic_term WITH { a }

%inline field_term:
  id=ID EQUAL t=simple_term { (id, t) }

literal:
  f=lfloat { Float f }
| i=lint   { Int i }
| c=LCHAR  { Char c }
| b=LBOOL  { Bool b }
| s=LSTRING{ String s }

lfloat:
  f=LFLOAT { f }
| LPAREN PLUS f=LFLOAT RPAREN { f }
| LPAREN MINUS f=LFLOAT RPAREN { -. f }

lint:
  i=LINT { i }
| LPAREN PLUS i=LINT RPAREN { i }
| LPAREN MINUS i=LINT RPAREN { Z.neg i }

%inline optional_typ:
| { None }
| COLON ty=typ { Some ty }

parameter:
  arg = ID { (None, PatVar arg) }
| LPAREN arg = pattern opta = optional_typ RPAREN
{ (opta, arg) }

generalized_identifier:
  | x=ID | x=OPID { x }

infix:
  | x=INFIX {x}
  | DIV   {"/"}
  | TIMES {"*"}
  | PLUS  {"+"}
  | MINUS {"-"}
  | EQUAL {"="}
  | LT    {"<"}
  | GT    {">"}
  | DOUBLEPOINT {".."}

prefix:
  | x=PREFIX {x}
  | INTERROGATION_MARK {"?"}
  | EXCLAMATION_MARK {"!"}
  | NEG {"~"}

(* ===== TYPES ===== *)

%inline param_type_def:
| name=ID EQUAL t=typ_norec { (name, [], t) }
| name=ID LPAREN params=separated_list(COMMA, TVAR) RPAREN EQUAL t=typ_norec { (name, params, t) }

typ:
  t=typ_norec { t }
| t=typ_norec WHERE ts=separated_nonempty_list(AND_KW, param_type_def)
  { TWhere (t, ts) }

typ_norec:
  t=simple_typ { t }
| hd=simple_typ COMMA tl=separated_nonempty_list(COMMA, simple_typ) { TTuple (hd::tl) }

simple_typ:
  t=atomic_typ { t }
| s=ID LPAREN ts=separated_list(COMMA, simple_typ) RPAREN { TApp(s, ts) }
| lhs=simple_typ ARROW rhs=simple_typ { TArrow (lhs, rhs) }
| lhs=simple_typ CONS rhs=simple_typ  { TCons (lhs, rhs) }
| NEG t=simple_typ { TNeg t }
| lhs=simple_typ OR rhs=simple_typ  { TCup (lhs, rhs) }
| lhs=simple_typ AND rhs=simple_typ { TCap (lhs, rhs) }
| lhs=simple_typ DIFF rhs=simple_typ  { TDiff (lhs, rhs) }

atomic_typ:
  x=type_constant { TBase x }
| s=ID { builtin_type_or_custom s }
| s=CID { TAtom s }
| s=PCID t=typ RPAREN { TTag (s, t) }
| s=PCID RPAREN { TTag (s, TBase TUnit) }
| s=TVAR { TVar s }
| s=TVAR_WEAK { TVarWeak s }
| LPAREN RPAREN { TBase TUnit }
| LPAREN t=typ RPAREN { t }
| LBRACE fs=separated_list(SEMICOLON, typ_field) o=optional_open RBRACE { TRecord (o, fs) }
| LBRACKET re=typ_re RBRACKET { TSList re }

%inline optional_open:
  { false }
| DOUBLEPOINT { true }

%inline typ_field:
  id=ID COLON t=simple_typ { (id, t, false) }
| id=ID COLON_OPT t=simple_typ { (id, t, true) }

%inline type_constant:
| i=tint { TInt (Some i, Some i) }
| LPAREN i1=tint? DOUBLEPOINT i2=tint? RPAREN { TInt (i1,i2) }
| c=LCHAR { TCharInt (c,c) }
| LPAREN c1=LCHAR MINUS c2=LCHAR RPAREN { TCharInt (c1,c2) }
| b=LBOOL { if b then TTrue else TFalse }
| str=LSTRING { TSString str }

tint:
  i=LINT { i }
// | PLUS i=LINT { i } // conflict with with regexp
| MINUS i=LINT { Z.neg i }

(* ===== REGEX ===== *)

typ_re:
| { Lists.Epsilon }
| re=nonempty_re { re }

nonempty_re:
| res=separated_nonempty_list(OR, simple_re) { Lists.Union res }

simple_re:
| res=nonempty_list(atomic_re) { Lists.Concat res }

atomic_re:
  t=atomic_typ { Lists.Symbol t }
| EXCLAMATION_MARK LPAREN re=nonempty_re RPAREN { re }
| re=atomic_re TIMES { Lists.Star re }
| re=atomic_re PLUS { Lists.Plus re }
| re=atomic_re INTERROGATION_MARK { Lists.Option re }

(* ===== PATTERNS ===== *)

%inline patterns:
  lst=separated_nonempty_list(OR, pat_line) {lst}
| OR lst=separated_nonempty_list(OR, pat_line) {lst}

%inline pat_line:
  p=pattern ARROW t=term { (p,t) }

%inline ppattern:
| LPAREN RPAREN { PatType (TBase TUnit) }
| LPAREN p=pattern RPAREN { p }

pattern:
  p=simple_pattern { p }
| hd=simple_pattern COMMA tl=separated_nonempty_list(COMMA, simple_pattern) { PatTuple (hd::tl) }

simple_pattern:
  a=simple_pattern_nocons { a }
| lhs=simple_pattern_nocons CONS rhs=simple_pattern { PatCons (lhs, rhs) }

simple_pattern_nocons:
  p=atomic_pattern { p }
| lhs=simple_pattern_nocons AND rhs=atomic_pattern { PatAnd (lhs, rhs) }
| lhs=simple_pattern_nocons OR rhs=atomic_pattern { PatOr (lhs, rhs) }

atomic_pattern:
  COLON t=atomic_typ { PatType t }
| v=ID  { PatVar v }
| c=literal { PatLit c }
| a=CID { PatType (TAtom a) }
| t=PCID p=pattern RPAREN { PatTag (t,p) }
| t=PCID RPAREN { PatType (TTag (t,TBase TUnit)) }
| LBRACE fs=separated_list(SEMICOLON, pat_field) o=optional_open RBRACE { PatRecord (fs, o) }
| LPAREN RPAREN { PatType (TBase TUnit) }
| LPAREN p=pattern RPAREN { p }
| v=ID EQUAL c=literal { PatAssign (v, c) }
| LBRACKET lst=separated_list(SEMICOLON, pattern) RBRACKET { list_of_pats lst }

%inline pat_field:
  id=ID EQUAL p=simple_pattern { (id, p) }
| id=ID { (id, PatVar id) }
