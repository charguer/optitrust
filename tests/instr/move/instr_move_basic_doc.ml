open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

  !! Instr_basic.move ~dest:[tAfter; cVarDef "b"] [cVarDef "c"];

)
