open Optitrust
open Target
open Ast
open CRawAst_to_ast

let test_elim_intro () = 
  let clang_ast = Clang.Ast.parse_file "c_elimintro.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in 
  Ast_check.check_transfo_is_identity ~test:"elim_intro" (fun t -> stackvar_intro (caddress_intro (caddress_elim (stackvar_elim t)))) raw_ast


let _ = test_elim_intro ()


let _ = Flags.dump_ast_details := true 

let _ = Run.script_cpp ~raw_ast:true (fun () ->
    
  !! Trace.apply stackvar_elim; (* Press F6 on this line *)
     Trace.apply caddress_elim; 
     Trace.apply caddress_intro;
     Trace.apply stackvar_intro;
)
