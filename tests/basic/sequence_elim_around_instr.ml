open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    
    !! Sequence_basic.elim_around_instr [cVarDef "x"];
    !! Sequence_basic.elim_around_instr [cVarDef "y"];
)
