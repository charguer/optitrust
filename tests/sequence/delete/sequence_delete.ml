open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.delete [sInstr "a++"];
  !! Sequence_basic.iter_delete [[cVarDef "a"]; [cVarDef "v"]];
  !! Sequence_basic.delete [nbMulti; sInstr "u."];
  !! Trace.failure_expected (fun () ->
       Sequence_basic.delete [nbMulti; cInt 8]);

)
