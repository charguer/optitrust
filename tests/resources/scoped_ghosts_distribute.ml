open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Resources.ghost_scopes_distribute [tBefore; nbMulti; sInstr "+= 1"];
)
