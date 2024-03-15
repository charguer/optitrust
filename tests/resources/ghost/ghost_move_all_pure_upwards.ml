open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
  !! iteri (fun i p -> Marks.add (Printf.sprintf "m%d" i) (target_of_path p)) [cFunBody "f"; tBetweenAll];
  !! Ghost.move_all_pure_upwards [cFunBody "f"];
)
