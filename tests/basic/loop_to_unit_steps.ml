open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.to_unit_steps ~index:"j" [cFor "i"];
  (* if not provided, the new index name is generated from the original one *)
  !! Trace.alternative (fun () ->
    !! Loop_basic.to_unit_steps [cFor "i"];
    !!(); )
  )
