open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.delete_void [cFor "i"];
  !! Loop_basic.delete_void [cFor "i3"];
  !! Loop_basic.delete_void [cFor "i2"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.delete_void [cFor "j"]
  );
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.delete_void [cFor "j2"]
  );
)
