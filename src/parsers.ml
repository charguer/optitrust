type cparser = Clang | Menhir | Default | All

let default_cparser = Clang

(* Reference to store the parser that is being currenty used *)
let selected_cparser = ref Default

let cparser_of_string (s : string) : cparser =
   match s with
    | "clang" -> Clang
    | "menhir" -> Menhir
    | "all" -> All
    | "default" -> Default
    | _ -> failwith "cparser_of_string: please chhose one of the following options, 'clang', 'Menhir', 'All'"

let string_of_parser (p : cparser) : string =
   match p with
    | Clang -> "clang"
    | Menhir -> "menhir"
    | All -> "all"
    | Default -> "default"


let get_selected ?(parser : cparser = Default) () : cparser =
  if parser <> Default then parser
  else if !selected_cparser <> Default then !selected_cparser
  else default_cparser

(* Change the current used cparser, unless the argument [p] is [Default] *)
let select_if_not_default (p : cparser) : unit =
  if p <> Default
    then selected_cparser := p

(* Select the parser from the command line *)
let set_selected_parser (s : string) : unit =
  selected_cparser := (cparser_of_string s)




