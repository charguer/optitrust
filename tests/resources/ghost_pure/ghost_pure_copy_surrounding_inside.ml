open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
  !! Ghost_pure.copy_surrounding_inside [cFor "i"];
  !! Ghost_pure.minimize_all_in_seq [cFunBody "f"];
)
