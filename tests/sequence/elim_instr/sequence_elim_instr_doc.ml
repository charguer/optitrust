open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.elim_instr [cLabel "toinline"];

)
