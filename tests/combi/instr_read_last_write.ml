open Optitrust
open Target

let _ = Run.script_cpp (fun _->

    !! Instr.read_last_write ~write:[cWrite ~rhs:[cInt 7] ()] [cRead ~addr:[cVar "x" ] ()];
    !! Instr.read_last_write ~write:[sInstr "t[0] ="] [sInstr "= t[0]"; dRHS];
)
