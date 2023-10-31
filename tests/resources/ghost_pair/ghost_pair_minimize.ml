open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Ghost_pair.minimize_all_in_seq [cFunBody ""];
  !! Resources.recompute_all_resources ();
)
