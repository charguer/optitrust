open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.move_out_alloc [cFunBody "alloc"; sInstr "m ="];

  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.move_out_alloc [cFunBody "var_wrong"; sInstr "x = 3"]);

  !! Resources.ensure_computed ();
)
