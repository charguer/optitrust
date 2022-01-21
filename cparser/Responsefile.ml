# 19 "lib/Responsefile.mll"
 
(* To accumulate the characters in a word or quoted string *)
let buf = Buffer.create 32

(* Add the current contents of buf to the list of words seen so far,
   taking care not to add empty strings unless warranted (e.g. quoted) *)
let stash inword words =
  if inword then begin
    let w = Buffer.contents buf in
    Buffer.clear buf;
    w :: words
  end else
    words


# 18 "lib/Responsefile.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\252\255\253\255\005\000\255\255\001\000\254\255\002\000\
    \255\255\003\000\254\255\255\255\004\000\254\255\255\255";
  Lexing.lex_backtrk =
   "\004\000\255\255\255\255\001\000\255\255\255\255\255\255\001\000\
    \255\255\002\000\255\255\255\255\002\000\255\255\255\255";
  Lexing.lex_default =
   "\255\255\000\000\000\000\255\255\000\000\006\000\000\000\008\000\
    \000\000\255\255\000\000\000\000\255\255\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \000\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\001\000\000\000\000\000\003\000\013\000\002\000\
    \000\000\000\000\010\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\255\255\255\255\011\000\014\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\000\000\003\000\003\000\
    \255\255\003\000\003\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\003\000\012\000\000\000\
    \255\255\255\255\009\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\005\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\005\000\007\000\009\000\012\000\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec gnu_unquoted inword words lexbuf =
   __ocaml_lex_gnu_unquoted_rec inword words lexbuf 0
and __ocaml_lex_gnu_unquoted_rec inword words lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 49 "lib/Responsefile.mll"
                  ( List.rev (stash inword words) )
# 118 "lib/Responsefile.ml"

  | 1 ->
# 50 "lib/Responsefile.mll"
                  ( gnu_unquoted false (stash inword words) lexbuf )
# 123 "lib/Responsefile.ml"

  | 2 ->
# 51 "lib/Responsefile.mll"
                  ( gnu_single_quote lexbuf; gnu_unquoted true words lexbuf )
# 128 "lib/Responsefile.ml"

  | 3 ->
# 52 "lib/Responsefile.mll"
                  ( gnu_double_quote lexbuf; gnu_unquoted true words lexbuf )
# 133 "lib/Responsefile.ml"

  | 4 ->
# 53 "lib/Responsefile.mll"
                  ( gnu_one_char lexbuf; gnu_unquoted true words lexbuf )
# 138 "lib/Responsefile.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_gnu_unquoted_rec inword words lexbuf __ocaml_lex_state

and gnu_one_char lexbuf =
   __ocaml_lex_gnu_one_char_rec lexbuf 5
and __ocaml_lex_gnu_one_char_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 56 "lib/Responsefile.mll"
               c
# 151 "lib/Responsefile.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 56 "lib/Responsefile.mll"
                  ( Buffer.add_char buf c )
# 155 "lib/Responsefile.ml"

  | 1 ->
let
# 57 "lib/Responsefile.mll"
         c
# 161 "lib/Responsefile.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 57 "lib/Responsefile.mll"
                  ( Buffer.add_char buf c )
# 165 "lib/Responsefile.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_gnu_one_char_rec lexbuf __ocaml_lex_state

and gnu_single_quote lexbuf =
   __ocaml_lex_gnu_single_quote_rec lexbuf 9
and __ocaml_lex_gnu_single_quote_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 60 "lib/Responsefile.mll"
                  ( () (* tolerance *) )
# 177 "lib/Responsefile.ml"

  | 1 ->
# 61 "lib/Responsefile.mll"
                  ( () )
# 182 "lib/Responsefile.ml"

  | 2 ->
# 62 "lib/Responsefile.mll"
                  ( gnu_one_char lexbuf; gnu_single_quote lexbuf )
# 187 "lib/Responsefile.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_gnu_single_quote_rec lexbuf __ocaml_lex_state

and gnu_double_quote lexbuf =
   __ocaml_lex_gnu_double_quote_rec lexbuf 12
and __ocaml_lex_gnu_double_quote_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 65 "lib/Responsefile.mll"
                  ( () (* tolerance *) )
# 199 "lib/Responsefile.ml"

  | 1 ->
# 66 "lib/Responsefile.mll"
                  ( () )
# 204 "lib/Responsefile.ml"

  | 2 ->
# 67 "lib/Responsefile.mll"
                  ( gnu_one_char lexbuf; gnu_double_quote lexbuf )
# 209 "lib/Responsefile.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_gnu_double_quote_rec lexbuf __ocaml_lex_state

;;

# 69 "lib/Responsefile.mll"
 

let re_responsefile = Str.regexp "@\\(.*\\)$"

exception Error of string

let expandargv argv =
  let rec expand_arg seen arg k =
    if not (Str.string_match re_responsefile arg 0) then
      arg :: k
    else begin
      let filename = Str.matched_group 1 arg in
      if List.mem filename seen then
        raise (Error ("cycle in response files: " ^ filename));
      let ic = open_in filename in
      let words = gnu_unquoted false [] (Lexing.from_channel ic) in
      close_in ic;
      expand_args (filename :: seen) words k
    end
  and expand_args seen args k =
    match args with
    | [] -> k
    | a1 :: al -> expand_args seen al (expand_arg seen a1 k)
  in
  let args = Array.to_list argv in
   Array.of_list (List.rev (expand_args [] args []))

(* This function reimplements quoting of writeargv from libiberty *)
let gnu_quote arg =
  let len = String.length arg in
  let buf = Buffer.create len in
  String.iter (fun c -> begin match c with
    | ' ' | '\t' | '\r' | '\n' | '\\' | '\'' | '"' ->
        Buffer.add_char buf '\\'
    | _ -> () end;
    Buffer.add_char buf c) arg;
  Buffer.contents buf

let re_quote = Str.regexp ".*[ \t\n\r\"]"

let diab_quote arg =
  let buf = Buffer.create ((String.length arg) + 8) in
  let doublequote = Str.string_match re_quote arg 0 in
  if doublequote then begin
    Buffer.add_char buf '"';
    String.iter (fun c ->
      if c = '"' then Buffer.add_char buf '\\';
      Buffer.add_char buf c) arg;
    if doublequote then Buffer.add_char buf '"';
    Buffer.contents buf
  end else
    arg

# 270 "lib/Responsefile.ml"
