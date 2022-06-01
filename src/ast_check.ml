open Ast

(* [check test_name t1 t2]: checks if [t1] and [t2] are equal or not, if not
    this function will print both the C code and the Optitrust ast.
    into files with prefix [test_name] *)
let check (test_name : string) (t1 : trm) (t2 : trm) : unit =
  (* LATER: see if we can have a ast comparison function *)
  let success = Ast_to_text.ast_to_string t1 = Ast_to_text.ast_to_string t2 in
  Printf.printf "Checking %s: %s\n" test_name (if success
    then "succcess"
    else Printf.sprintf "failure (see %s_ast_{1,2}.{ast,cpp})" test_name);
  if not success then begin
      AstC_to_c.ast_to_file (test_name ^ "_ast_1.cpp") t1;
      AstC_to_c.ast_to_file (test_name ^ "_ast_2.cpp") t2;
      Ast_to_text.ast_to_file (test_name ^ "_ast_1.ast") t1;
      Ast_to_text.ast_to_file (test_name ^ "_ast_2.ast") t2;
  end

(* [check_transfo_is_identity ~test f t]: checks if ft = t *)
let check_transfo_is_identity ?(test : string = "") (f : trm -> trm) (t : trm) : unit =
  let t1 = f t in
  check test t t1