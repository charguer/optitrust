open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Sequence.iter_delete [[cVarDef "a"]; [cVarDef "v"]];
   !! Sequence.delete [nbMulti; sInstrRegexp "u\\.."];
   !! Tools.failure_expected (fun () ->
        Sequence.delete [nbMulti; cInt 0]);
)
