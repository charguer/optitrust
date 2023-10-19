open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.unswitch [cIf ~cond:[sExpr "b"] ()];

)
