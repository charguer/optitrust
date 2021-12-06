open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.simpl_deref [cRead ~addr:[cVar "b"] ()];
  !! Variable_basic.simpl_deref [cRead ~addr:[cVar "a"] ()];
)
