open Optitrust
open Target



let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.ref_to_pointer [cVarDef "b"];

)
