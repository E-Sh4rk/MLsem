%{ (* Emacs, use -*- tuareg -*- to open this file. *)

  open Ast
  open Types.Additions
  open Types.Base
  open Sstt.Extensions

  let annot sp ep e =
    (Ast.new_annot (Position.lex_join sp ep), e)

  let tmp_var = "__encoding"
  let multi_param_abstraction startpos endpos lst t =
    let step acc (annotation, pat) =
      match pat with
      | PatVar v -> annot startpos endpos (Lambda (annotation, v, acc))
      | pat ->
        let test = annot startpos endpos (Var tmp_var) in
        let body = annot startpos endpos (PatMatch (test, [(pat, acc)])) in
        annot startpos endpos (Lambda (annotation, tmp_var, body))
    in
    List.rev lst |> List.fold_left step t

  let fresh_tvar_id =
    let last = ref 0 in
    (fun () ->
      last := (!last) + 1 ;
      Format.sprintf "__self_%i" (!last))
  let multi_param_rec_abstraction startpos endpos name lst oty t =
    let rec aux lst =
      match lst with
      | [] ->
        begin match oty with
        | None -> TVarWeak (fresh_tvar_id ())
        | Some ty -> ty
        end
      | (Unnanoted, _)::lst -> TArrow (TVarWeak (fresh_tvar_id ()), aux lst)
      | (ADomain [ty], _)::lst -> TArrow (ty, aux lst)
      | (ADomain _, _)::_ -> failwith "Parameters of recursive functions cannot have multiple type annotations."
    in
    let self_annot = aux lst in
    let lst = (ADomain [self_annot], PatVar name)::lst in
    let t = multi_param_abstraction startpos endpos lst t in
    annot startpos endpos (Fixpoint t)

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
%token FUN LET REC TOPLEVEL IN FST SND HD TL DEBUG
%token IF IS THEN ELSE
%token LPAREN RPAREN EQUAL COMMA CONS COLON COLON_OPT COERCE INTERROGATION_MARK EXCLAMATION_MARK
%token ARROW AND OR NEG DIFF
%token TIMES PLUS MINUS DIV
%token LBRACE RBRACE DOUBLEPOINT MATCH WITH END POINT LT GT
%token TYPE TYPE_AND WHERE ABSTRACT
%token LBRACKET RBRACKET SEMICOLON
%token<string> ID CID PCID
%token<string> TVAR TVAR_WEAK
%token<float> LFLOAT
%token<Z.t> LINT
%token<bool> LBOOL
%token<char> LCHAR
%token<string> LSTRING
%token MAGIC
%token<string> INFIX PREFIX

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

element:
  i=optional_debug LET id=generalized_identifier COLON ty=typ EQUAL t=term
  { annot $symbolstartpos $endpos (Definition (i, (id, t, Some ty))) }
| i=optional_debug LET gioa=gen_id_opt_annot ais=parameter* EQUAL t=term
  { 
    let (id, ty) = gioa in
    let t = multi_param_abstraction $startpos $endpos ais t in
    annot $symbolstartpos $endpos (Definition (i, (id, t, ty)))
  }
| i=optional_debug LET REC gioa=gen_id_opt_annot ais=parameter* oty=optional_typ EQUAL t=term
  { 
    let (id, ty) = gioa in
    let t = multi_param_rec_abstraction $startpos $endpos id ais oty t in
    annot $symbolstartpos $endpos (Definition (i, (id, t, ty)))
  }
| TYPE ts=separated_nonempty_list(TYPE_AND, param_type_def) { annot $symbolstartpos $endpos (Types ts) }
| ABSTRACT TYPE name=ID params=abs_params { annot $symbolstartpos $endpos (AbsType (name, params)) }

%inline abs_params:
  { [] }
| LPAREN vs=separated_nonempty_list(COMMA, variance) RPAREN { vs }

variance:
  TVAR { Inv }
| PLUS TVAR { Cov }
| MINUS TVAR { Cav }

%inline gen_id_opt_annot:
  id=generalized_identifier { (id, None) }
| LPAREN id=generalized_identifier oty=optional_typ RPAREN { (id, oty) }

%inline optional_typ:
| { None }
| COLON ty=typ { Some ty }

%inline optional_debug:
  { Utils.log_disabled }
| DEBUG { Utils.log_full }
| DEBUG i=lint { Z.to_int i }

(* ===== TERMS ===== *)

%inline optional_test_type:
  { TBase TTrue }
| IS t=typ { t }

term:
  t=simple_term { t }
| FUN ais=parameter+ ARROW t = term { multi_param_abstraction $startpos $endpos ais t }
| LET id=generalized_identifier ais=parameter* EQUAL td=term IN t=term
  {
    let td = multi_param_abstraction $startpos $endpos ais td in
    annot $startpos $endpos (Let (id, td, t))
  }
| LET REC id=generalized_identifier ais=parameter* oty=optional_typ EQUAL td=term IN t=term
  { 
    let td = multi_param_rec_abstraction $startpos $endpos id ais oty td in
    annot $symbolstartpos $endpos (Let (id, td, t))
  }
| LET TOPLEVEL id=generalized_identifier ais=parameter* EQUAL td=term IN t=term
  { 
    let td = multi_param_abstraction $startpos $endpos ais td in
    let td = annot $symbolstartpos $endpos (TopLevel td) in
    annot $symbolstartpos $endpos (Let (id, td, t))
  }
| LET LPAREN p = pattern RPAREN EQUAL td=term IN t=term { let_pattern $startpos $endpos p td t }
| IF t=term ott=optional_test_type THEN t1=term ELSE t2=term { annot $startpos $endpos (Ite (t,ott,t1,t2)) }
| MATCH t=term WITH pats=patterns END { annot $startpos $endpos (PatMatch (t,pats)) }
| hd=simple_term COMMA tl=separated_nonempty_list(COMMA, simple_term) { annot $startpos $endpos (Tuple (hd::tl)) }

simple_term:
  a=simple_term_nocons { a }
| lhs=simple_term_nocons CONS rhs=simple_term { annot $startpos $endpos (Cons (lhs, rhs)) }

simple_term_nocons:
  a=simple_term_nocons_noapp { a }
| a=simple_term_nocons b=simple_term_nocons_noapp { annot $startpos $endpos (App (a, b)) }
| p=proj a=simple_term_nocons_noapp { annot $startpos $endpos (Projection (p, a)) }
| a=simple_term_nocons_noapp s=infix_term b=simple_term_nocons_noapp { double_app $startpos $endpos s a b }
| p=prefix_term a=simple_term_nocons_noapp { annot $startpos $endpos (App (p, a)) }
| LT t=typ GT { annot $startpos $endpos (Abstract t) }

simple_term_nocons_noapp:
  a=atomic_term { a }
| a=atomic_term POINT id=ID { annot $startpos $endpos (Projection (Field id, a)) }
| a=atomic_term DIFF id=ID { annot $startpos $endpos (RecordUpdate (a,id,None)) }

proj:
| FST { Pi(2,0) } | SND { Pi(2,1) } | HD { Hd } | TL { Tl }

infix_term:
  x=infix { annot $startpos $endpos (Var x) }

prefix_term:
  x=prefix { annot $startpos $endpos (Var x) }

atomic_term:
  x=generalized_identifier { annot $startpos $endpos (Var x) }
| c=CID { annot $startpos $endpos (Atom c) }
| t=PCID a=term RPAREN { annot $startpos $endpos (Tag (t,a)) }
| t=PCID RPAREN { annot $startpos $endpos (Tag (t,annot $startpos $endpos (Const Unit))) }
| l=literal { annot $startpos $endpos (Const l) }
| MAGIC { annot $startpos $endpos (Abstract (TBase TEmpty)) }
| LPAREN RPAREN { annot $startpos $endpos (Const Unit) }
| LPAREN t=term RPAREN { t }
| LPAREN t=term COLON tys=separated_nonempty_list(SEMICOLON, typ) RPAREN { annot $startpos $endpos (TypeConstr (t,tys)) }
| LPAREN t=term COERCE tys=separated_nonempty_list(SEMICOLON, typ) RPAREN { annot $startpos $endpos (TypeCoercion (t,tys)) }
| LBRACE obr=optional_base_record fs=separated_list(SEMICOLON, field_term) RBRACE
{ record_update $startpos $endpos obr fs }
| LBRACKET lst=separated_list(SEMICOLON, term) RBRACKET
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

parameter:
  arg = ID { (Unnanoted, PatVar arg) }
| LPAREN arg = pattern opta = optional_param_type_annot RPAREN
{ (opta, arg) }

%inline optional_param_type_annot:
    { Unnanoted }
  | COLON tys = separated_nonempty_list(SEMICOLON, typ) { ADomain tys }

generalized_identifier:
  | x=ID | LPAREN x=prefix RPAREN | LPAREN x=infix RPAREN { x }

infix:
  | x=INFIX {x}
  | DIV   {"/"}
  | TIMES {"*"}
  | PLUS  {"+"}
  | MINUS {"-"}
  | EQUAL {"="}
  | LT    {"<"}
  | GT    {">"}

prefix:
  | x=PREFIX {x}
  | INTERROGATION_MARK {"?"}
  | EXCLAMATION_MARK {"!"}

(* ===== TYPES ===== *)

%inline param_type_def:
| name=ID EQUAL t=typ_norec { (name, [], t) }
| name=ID LPAREN params=separated_list(COMMA, TVAR) RPAREN EQUAL t=typ_norec { (name, params, t) }

typ:
  t=typ_norec { t }
| t=typ_norec WHERE ts=separated_nonempty_list(TYPE_AND, param_type_def)
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
| a=CID { PatType (TAtom a) }
| t=PCID p=pattern RPAREN { PatTag (t,p) }
| t=PCID RPAREN { PatType (TTag (t,TBase TUnit)) }
| LPAREN RPAREN { PatType (TBase TUnit) }
| LBRACE fs=separated_list(SEMICOLON, pat_field) o=optional_open RBRACE { PatRecord (fs, o) }
| LPAREN p=pattern RPAREN { p }
| v=ID EQUAL c=literal { PatAssign (v, c) }
| LBRACKET lst=separated_list(SEMICOLON, pattern) RBRACKET { list_of_pats lst }

%inline pat_field:
  id=ID EQUAL p=simple_pattern { (id, p) }
| id=ID { (id, PatVar id) }
