open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.to_unit_steps ~index:"j" [cFor "i"];
  (* FIX: new_index -> index *)
  (* FIX: example with no new_index: *)
     !! Trace.alternative (fun () ->
        !! Loop.to_unit_steps [cFor "i"];
        !!(); )
  )
