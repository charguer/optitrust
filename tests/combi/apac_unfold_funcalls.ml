open Optitrust
open Target 
open Ast
open Apac_core

let _ = Run.script_cpp (fun () -> 

  Ast_data.fill_fun_defs_tbl (get_ast());
  
  !! Apac.unfold_funcalls [cFunDef "h"];
)