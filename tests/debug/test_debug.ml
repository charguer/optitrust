open Optitrust
open Target



let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   
   !! Variable_basic.init_detach [cVarDef "p"];
   
)
