open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->

  !! Loop.unroll_first_iterations 2 [cFor "i"];

)
