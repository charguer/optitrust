open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp(fun _ ->
  !! Loop.delete_void [cFor "i"];
  !! Loop.delete_void ~nest_of:2 [cFor "i2"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop.delete_void [cFor "j"]
  );
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop.delete_void ~nest_of:2 [cFor "j2"]
  );
  !! Loop.delete_all_void [];
)
