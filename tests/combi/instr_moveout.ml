open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Instr.move_out [cVarDefs ["b";"d"]];
    !! Instr.move_out [cFor "i"];
)