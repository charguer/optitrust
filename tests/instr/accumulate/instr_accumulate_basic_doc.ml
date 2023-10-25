open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Instr_basic.accumulate [cLabel "fuse"];

)
