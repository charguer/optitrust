open Optitrust
open Target


let _ = Run.script_cpp (fun _ -> 

    !! Arith_basic.simplify [occFirst; cWriteVar "x"; dRHS];

)