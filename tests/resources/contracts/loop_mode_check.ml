open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ = Flags.recompute_resources_between_steps := false

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();
  !! Omp_basic.parallel_for [cFor "i"];
  !! Loop.tile (trm_int 4) ~index:"ii" ~bound:TileDivides [cFor "i"];
  !! Trace.failure_expected (function
  | Resource_computation.ResourceError _ -> true
  | _ -> false) (fun () ->
    Resources.ensure_computed ());
  );
