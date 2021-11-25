open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

    !! Arith_basic.simplify [occIndex 1; cWriteVar "x"; dRHS]; (* occFirst*)

)