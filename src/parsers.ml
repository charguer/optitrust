type cparser = Clang | Menhir | Default | All 

let cparser_of_string (s : string) : cparser = 
  match s with 
  | "clang" -> Clang 
  | "menhir" -> Menhir
  | "default" -> Default
  | "all" -> All
  | _ -> failwith "cparser_of_string: please chhose one of the following options, 'clang', 'Menhir', 'Defaul', 'All'"