open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Record_basic.set_implicit ~keep_label:false [cLabel "fuse"];

)
