open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop.swap [cFunBody "g"; cFor "a"];
  !! Loop.swap [cFunBody "g"; cFor "a"];
  !! Loop.swap [cFunBody "g"; cFor "c"];
  !! Loop.swap [cFunBody "g"; cFor "b"];

  !! Trace.failure_expected (fun () ->
    Loop.swap [cFunBody "g"; cFor "i"]);

  !! Loop.swap [cFunBody "f"; cFor "x"];
)
