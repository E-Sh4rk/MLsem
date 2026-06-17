open Mlsem_common
open Lexing

let pos_of_lexbuf lexbuf =
  let pos = lexbuf.lex_curr_p in
  Position.lex_join pos pos

let parse_with_errors parser buf =
  try parser Lexer.token buf with
  | Lexer.LexerError msg ->
    raise (PAst.LexicalError (pos_of_lexbuf buf, msg))
  | Lexer.Parser.Error ->
    raise (PAst.SyntaxError (pos_of_lexbuf buf, "syntax error"))

let parse_file f source_filename =
  let cin = open_in source_filename in
  let buf = from_channel cin in
  buf.lex_curr_p <- { buf.lex_curr_p with  pos_fname = source_filename };
  parse_with_errors f buf

let parse_string f str =
  let buf = from_string str in
  buf.lex_curr_p <- { buf.lex_curr_p with  pos_fname = "_" };
  parse_with_errors f buf

let parse_type_file = parse_file Lexer.Parser.unique_ty
let parse_type_string = parse_string Lexer.Parser.unique_ty
let parse_expr_file = parse_file Lexer.Parser.unique_term
let parse_expr_string = parse_string Lexer.Parser.unique_term
let parse_program_file = parse_file Lexer.Parser.program
let parse_program_string = parse_string Lexer.Parser.program
