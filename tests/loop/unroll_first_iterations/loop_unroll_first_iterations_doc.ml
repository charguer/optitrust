open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  !! Loop.unroll_first_iterations 2 [cFor "i"];

)
