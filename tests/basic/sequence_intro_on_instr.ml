open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Sequence_basic.intro_on_instr [cVarDef "x"];
    !! Sequence_basic.intro_on_instr [sInstr "y = 1"];
)
