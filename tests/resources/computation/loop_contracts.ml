open Optitrust
open Target

let _ = Flags.check_validity := true
(*let _ = Flags.resource_errors_as_warnings := true*)

let _ = Run.script_cpp (fun () ->
  Resources.show ();

  !! Trace.failure_expected (fun () ->
    Omp.parallel_for [cFunDef "array_copy_explicit"; cFor "i"]);

  !! Omp.parallel_for [cFunDef "array_copy_par"; cFor "i"];
)
