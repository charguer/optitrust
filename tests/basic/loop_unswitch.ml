open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.unswitch [cIf ~cond:[cBool true] ()];
  !! Loop_basic.unswitch [cIf ~cond:[cBool false] ()];
)
