open Optitrust
open Target


let _ = Run.script_cpp (fun () ->
  !! Loop.fission ~nest_of:2 [nbExact 1; tAfter; cFor "i"; cFor "j"; cWriteVar "y"];
  !! Loop.fission ~nest_of:2 [cForBody "k"; tBetweenAll];
)
