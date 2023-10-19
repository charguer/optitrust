open Optitrust
open Target


let _ = Run.script_cpp (fun () ->
  !! Loop.fission ~nest_of:2 [nbExact 1; tAfter; cFor "i"; cFor "j"; cWriteVar "y"];
  !! Loop.fission_all_instrs ~nest_of:2 [cFor ~body:[cFor "k"] "i"];
)
