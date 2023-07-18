open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.resource_errors_as_warnings := true

let _ = Run.script_cpp (fun () ->
  show_res ();

  !! Trace.failure_expected (fun () ->
    Omp.parallel [cFunDef "array_copy_explicit"; cFor "i"]);

  !! Omp.parallel [cFunDef "array_copy_par"; cFor "i"];
)
