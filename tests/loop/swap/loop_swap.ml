open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Loop.swap [cFor "a"];
  !! Loop.swap [cFor "a"];
  !! Loop.swap [cFor "c"];
  !! Loop.swap [cFor "b"];

  !! Trace.failure_expected (fun () ->
    Loop.swap [cFor "i"]);

)
