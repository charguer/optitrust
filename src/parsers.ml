type cparser = Clang | Menhir | Default | All

let default_cparser = Clang


(* Option to select the C parser to use *)
let selected_cparser = ref Default

(* TODO:
let cparser_of_string (s : string) : cparser =
   match s with
    | "clang" -> Clang
    | "menhir" -> Menhir
    | "all" -> All
    | _ -> failwith "cparser_of_string: please chhose one of the following options, 'clang', 'Menhir', 'All'"

let set_selected_parser (s : string) : unit =
  selected_cparser := (cparser_of_string s)

  use set_selected_parser in flags
*)

let cparser_of_string (s : string) : unit =
  selected_cparser :=
    match s with
    | "clang" -> Clang
    | "menhir" -> Menhir
    | "all" -> All
    | _ -> failwith "cparser_of_string: please chhose one of the following options, 'clang', 'Menhir', 'All'"

