let string_arg_printer = Printf.sprintf "%S"

let option_arg_printer sub_printer x =
  match x with
  | None -> "None"
  | Some x -> Printf.sprintf "(Some %s)" (sub_printer x)

let list_arg_printer elt_printer l =
  Printf.sprintf "[%s]" (String.concat "; " (List.map elt_printer l))

