open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop.move_out ~upto:"i" [cVarDef "x"];

  !! Loop.move_out [cVarDef "s"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop.move_out [cVarDef "s"]);

  !! Trace.restore_original();
  !! Loop.move_out [cVarDef "x"];
  !! Loop.move_out [cVarDef "x"];

)
