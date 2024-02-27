open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Loop.unroll [cFor "i"];
  !! Loop.unroll [cFor "j"];

  !! Loop.unroll ~nest_of:2 [cFor "k"];

)
