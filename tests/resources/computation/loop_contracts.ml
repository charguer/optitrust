open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
(*let _ = Flags.resource_errors_as_warnings := true*)

let _ = Run.script_cpp (fun () ->
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Omp.parallel_for [cFunDef "array_copy_explicit"; cFor "i"]);

  !! Omp.parallel_for [cFunDef "array_copy_par"; cFor "i"];
)
