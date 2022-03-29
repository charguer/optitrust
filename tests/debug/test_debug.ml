open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   !! Instr.move_out ~dest:[tFirst; cTopFunDef "test";dBody] [cVarDef "a"];
   
)
 