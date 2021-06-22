open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   (* TODO: Sequence.remove p
      similar implementation to Sequence.sub
      Sequence.iter_remove ps
    *)

   (* Generic.remove_instruction [cMulti; cInt 0]; -> should raise error *)
   !! Generic.remove_instructions [[cVarDef "a"]; [cVarDef "v"]];
   !! Generic.remove_instruction [cMulti; cInstrRegexp "u\\.."];
)
