open Optitrust
open Run 


let _ = run_unit_test (fun () ->
  Arrays.to_variables ["ua";"ub"] [cVarDef "u"];
  Arrays.to_variables ["va";"vb"] [cVarDef "v"];
)
