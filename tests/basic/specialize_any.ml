open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  show [cAny];
  !! Specialize_basic.any "2" [nbMulti;cAny];
)