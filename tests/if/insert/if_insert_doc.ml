open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! If_basic.insert ~cond:(expr "x > 0") [sInstr "x++"];

)
