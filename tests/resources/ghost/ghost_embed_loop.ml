open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Ghost.embed_loop [nbMulti; cFor "i"];
  !! Resources.recompute_all_resources ()
)

