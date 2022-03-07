open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

  
    show [cVarDef "a"];


)

(* Note: recall that currently const references are not supported,
   see Ast_fromto_AstC *)
