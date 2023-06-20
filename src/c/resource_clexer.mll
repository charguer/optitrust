{
  open Resource_cparser
  open Lexing

  exception SyntaxError of string
}

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule lex_resources = parse
  | blank { lex_resources lexbuf }
  | newline { new_line lexbuf; lex_resources lexbuf }
  | ident { IDENT (lexeme lexbuf) }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | ',' { COMMA }
  | "->" { ARROW }
  | "=>" { FAT_ARROW }
  | "fun" { FUN }
  | '(' { LPAR }
  | ')' { RPAR }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }
