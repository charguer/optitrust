open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  show [cFun "CHOOSE"];
  !! Specialize_basic.choose "xa" [cChoose];
)