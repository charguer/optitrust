open Optitrust
open Target

let _ = Flags.check_validity := false

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.fission_basic [tBefore; sInstr "y +="];

)
