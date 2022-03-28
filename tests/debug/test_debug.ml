open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   !! Sequence.insert (stmt "printf(\"Hello\n\")") [tBefore; cVarDef "a"];
   !! Sequence.insert (stmt "printf(\"Hello\n\")") [tAfter; cVarDef ""];
   
)
 