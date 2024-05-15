open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();
  !! iteri (fun i p -> Marks.add (Printf.sprintf "m%d" i) (target_of_path p)) [cFunBody "f"; tBetweenAll];
  !! Ghost_pure.minimize_all_in_seq [cFunBody "f"];
  !! Ghost_pure.minimize_all_in_seq [cFunBody "g"; cForBody "i"];
  !! Ghost_pure.minimize_all_in_seq [cFunBody "h"; cIf (); cThen];
  !! Ghost_pure.minimize_all_in_seq [cFunBody "h2"; cIf (); cThen];
)
