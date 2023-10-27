open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Ghost_pair.fission [tBefore; nbMulti; sInstr "+= 1"];
)
