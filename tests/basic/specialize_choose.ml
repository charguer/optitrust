open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Specialize_basic.choose "xa" [cChoose];
)