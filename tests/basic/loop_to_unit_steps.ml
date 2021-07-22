open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.to_unit_steps ~new_index:"j" [cFor "i"];
  (* TODO: new_index -> index *)
  (* TODO: example with no new_index:
     !! Trace.alternative (fun () ->
        !! Loop.to_unit_steps [cFor "i"];
        !!(); ) *)
  )
