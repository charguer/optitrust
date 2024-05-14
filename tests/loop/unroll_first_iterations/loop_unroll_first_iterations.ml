open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  !! Loop.unroll_first_iteration [cFor "i"];
  !! Loop.unroll_first_iteration [cFor "j"];
  !! Loop.unroll_first_iteration [cFor "k"];
  !! Loop.unroll_first_iteration [cFor "l"];

  (* TODO: add more tests for unroll_first_iterations *)
)
