open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Sequence.iter_delete [[cVarDef "a"]; [cVarDef "v"]];
   !! Sequence.delete [cMulti; cInstrRegexp "u\\.."];
   !! Tools.failure_expected (fun () ->
        Sequence.delete [cMulti; cInt 0]);
)
