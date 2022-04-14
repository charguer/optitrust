(* List of parsers available *)
type cparser =
  | Clang (* use Clang parser, via its ClangML interface *)
  | Menhir (* use CompCert's Menhir parser *)
  | Default (* use [default_cparser], define below *)
  | All (* use all parsers, and ensure consistency of the results *)

(* [cparser_of_string s] return the parser that matches string [s] *)
let cparser_of_string (s : string) : cparser =
   match s with
    | "clang" -> Clang
    | "menhir" -> Menhir
    | "all" -> All
    | "default" -> Default
    | _ -> failwith "cparser_of_string: please chhose one of the following options, 'clang', 'Menhir', 'All'"

(* [string_of_cparser p] print parser [p] *)
let string_of_cparser (p : cparser) : string =
   match p with
    | Clang -> "clang"
    | Menhir -> "menhir"
    | All -> "all"
    | Default -> "default"

(* Default parser to use when no indication is provided,
   neither via the command line, nor via [Parsers.select],
   nor via the [~parser] option provided by functions such as
   [Run.script_cpp] or [Trace.reparse]. *)
let default_cparser = Clang

(* Reference to the selected parser *)
let selected_cparser = ref Default

(* Get the parser currently selected *)
let get_selected ?(parser : cparser = Default) () : cparser =
  if parser <> Default then parser
  else if !selected_cparser <> Default then !selected_cparser
  else default_cparser

(* Select [p] as parser to be used *)
let select (p : cparser) : unit =
  selected_cparser := p

(* Select the parser with name [s] as parser to be used *)
let select_by_string (s : string) : unit =
  select (cparser_of_string s)

(* Select [p] as parser to be used, unless [p] is [Default], in this case do nothing *)
let select_if_not_default (p : cparser) : unit =
  if p <> Default then select p




