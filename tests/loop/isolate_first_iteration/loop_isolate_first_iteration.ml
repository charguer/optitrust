open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  !! Loop.isolate_first_iteration [cFor "i"];
  !! Loop.isolate_first_iteration [cFor "j"];
  !! Loop.isolate_first_iteration [cFor "k"];
  !! Loop.isolate_first_iteration [cFor "l"];

)