open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Resources.scoped_ghosts_minimize [cFunBody ""];
  !! Resources.recompute_all_resources ();
)
