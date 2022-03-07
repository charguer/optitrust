open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

  
    Variable_basic.to_const [cFunDef "test_bag"; cVarDef "p"];


)

(* Note: recall that currently const references are not supported,
   see Ast_fromto_AstC *)
