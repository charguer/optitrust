open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   (* Sequence.delete [cMulti; [cMulti; cInt 0]] -> should raise error *)
   (* !!Sequence.iter_delete [[cVarDef "a"]; [cVarDef "v"]]; *)
  (* TODO: Ask Arthur about this *)
   Sequence.delete [cMulti; cInstrRegexp "u\\.."];
)
