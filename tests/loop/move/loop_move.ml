open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop.move [occFirst; cFor "y"] ~before:[cFor "x"];

  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop.move [occFirst; cFor "y"] ~before:[cFor "cx"]);

)
