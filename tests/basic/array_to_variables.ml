open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Arrays_basic.to_variables ["ua";"ub"] [cVarDef "u"];
  !! Arrays_basic.to_variables ["va";"vb"] [cVarDef "v"];
)
