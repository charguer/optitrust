open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->

  !! Loop.unroll_first_iteration [cFor "i"];
  !! Loop.unroll_first_iteration [cFor "j"];
  !! Loop.unroll_first_iteration [cFor "k"];
  !! Loop.unroll_first_iteration [cFor "l"];

  (* TODO: add more tests for unroll_first_iterations *)
)
