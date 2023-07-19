{
  open Resource_cparser
  open Lexing

  exception SyntaxError of string
}

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_lit = ['0'-'9']+
let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule lex_resources = parse
  | ';' { SEMICOLON }
  | ':' { COLON }
  | ',' { COMMA }
  | '&' { AMPERSAND }
  | "->" { ARROW }
  | "~>" { SQUIG_ARROW }
  | "fun" { FUN }
  | '(' { LPAR }
  | ')' { RPAR }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | blank { lex_resources lexbuf }
  | newline { new_line lexbuf; lex_resources lexbuf }
  | ident { IDENT (lexeme lexbuf) }
  | int_lit { INT_LIT (int_of_string (lexeme lexbuf)) }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }
