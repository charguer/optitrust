(* TODO: this file should be name Parser, not Parsers.
  By default, always use singular names, they are shorter;
  (Flags is an exception, probably because it's an idiomatic plural word, like settings). *)


type cparser = Clang | Menhir | Default | All

let cparser_of_string (s : string) : cparser =
  match s with
  | "clang" -> Clang
  | "menhir" -> Menhir
  | "all" -> All
  | _ -> failwith "cparser_of_string: please chhose one of the following options, 'clang', 'Menhir', 'All'"