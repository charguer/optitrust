open Optitrust
open Target

let _ = Run.script_cpp (fun _->

    !! Instr.inline_last_write ~write:[cWrite ~rhs:[cInt 7] ()] [cRead ~addr:[cVar "x" ] ()];
)
