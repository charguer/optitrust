open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.init_attach [cVarDef "x"];
  !! Trace.failure_expected (fun _e -> true) (fun _ ->
      Variable_basic.init_attach [cVarDef "z"])

)
