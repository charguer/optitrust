

(* Main *)

let _ =
  let sourcename =
    if Array.length Sys.argv >= 2
      then Sys.argv.(1)
      else failwith "Please provide the input filename as argument"
    in
  (* Parse the ast *)
  let ast = MenhirC.parse_c_file_without_includes sourcename in
  (* Print the ast *)
  let destname = MenhirC.get_parsed_ast_filename sourcename in
  MenhirC.print_ast_to_file destname ast;
  Printf.printf "Produced %s\n" destname


(*
# make -C .. -f Makefile.extr depend
*)

