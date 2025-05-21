{
  open Resource_cparser
  open Lexing

  exception SyntaxError of string
}

let ident = ['a'-'z' 'A'-'Z' '_' '#'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_lit = ['0'-'9']+
let float_lit = ['0'-'9']+ '.' ['0'-'9']* (['e' 'E'] ['+' '-']? ['0'-'9']+)? 'f'
let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule lex_resources = parse
  | ':' { COLON }
  | ',' { COMMA }
  | '&' { AMPERSAND }
  | "->" { ARROW }
  | "~>" { SQUIG_ARROW }
  | "~~>" { LONG_SQUIG_ARROW }
  | "fun" { FUN }
  | "for" { FOR }
  | "forall" { FORALL }
  | "in" { IN }
  | ":=" { COLON_EQUAL }
  | "<-" { REV_ARROW }
  | '(' { LPAR }
  | ')' { RPAR }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '*' { STAR }
  | '/' { SLASH }
  | '%' { PERCENT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '=' { EQUAL }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "<>" { NEQ }
  | "." { DOT }
  | ".." { DOTDOT }
  | "_" { UNDERSCORE }
  | blank { lex_resources lexbuf }
  | newline { new_line lexbuf; lex_resources lexbuf }
  | ident { IDENT (lexeme lexbuf) }
  | int_lit { INT_LIT (int_of_string (lexeme lexbuf)) }
  | float_lit {
    let num = lexeme lexbuf in
    FLOAT_LIT (float_of_string (String.sub num 0 (String.length num - 1))) }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }
