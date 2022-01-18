open Optitrust
open Target
open Ast
open Ast_to_rawC
open CRawAst_to_ast

(* src/ast_check.ml *)
let check (test_name : string) (t1 : trm) (t2 : trm) : unit =
  (* LATER: see if we can have a ast comparison function *)
  let success = Ast_to_text.ast_to_string t1 = Ast_to_text.ast_to_string t2 in
  Printf.printf "Checking %s: %s\n" test_name (if success then "succcess" else Printf.sprintf "failure (see %s_ast_{1,2}.{ast,cpp})" test_name);
  if not success then begin
      Ast_to_rawC.ast_to_file (test_name ^ "_ast_1.cpp") t1;
      Ast_to_rawC.ast_to_file (test_name ^ "_ast_2.cpp") t2;
      Ast_to_text.ast_to_file (test_name ^ "_ast_1.ast") t1;
      Ast_to_text.ast_to_file (test_name ^ "_ast_2.ast") t2;
  end

let check_transfo_is_identity ?(test : string = "") (f : trm -> trm) (t : trm) : unit =
  let t1 = f t in
  check test t t1

let test_stackvar () =
  let clang_ast = Clang.Ast.parse_file "c_stackvar.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in
  check_transfo_is_identity ~test:"Stack variables" (fun t -> stackvar_intro (stackvar_elim t)) raw_ast

let _ = test_stackvar ()

let _ = Run.script_cpp ~raw_ast:true (fun () ->
  !! Trace.apply stackvar_elim;
  !! Trace.apply stackvar_intro;
 )
