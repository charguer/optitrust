open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.change_type "vect2" [cVarDef "w"];

)
