type cparser = Clang | Menhir | Default | All

let default_cparser = ref Clang

(* Reference to store the parser that is being currenty used *)
let selected_cparser = ref Default

let cparser_of_string (s : string) : cparser =
   match s with
    | "clang" -> Clang
    | "menhir" -> Menhir
    | "all" -> All
    | "default" -> Default
    | _ -> failwith "cparser_of_string: please chhose one of the following options, 'clang', 'Menhir', 'All'"

(* Change the current used cparser *)
let set_selected_parser (s : string) : unit =
  selected_cparser := (cparser_of_string s)




