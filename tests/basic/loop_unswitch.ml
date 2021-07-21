open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.unswitch [cIf ~cond:[cBool true] ()];
  !! Loop.unswitch [cIf ~cond:[cBool false] ()];
)
