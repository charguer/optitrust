open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->
  !! Function.uninline ~f:[cTopFunDef "f"] [cVarDef "b"];
  !! Function.uninline ~f:[cFunDef "mindex_fold"] [cFunDef "test_mindex_fold";cArrayWrite "x"];
)
