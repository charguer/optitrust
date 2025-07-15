open Optitrust
open Prelude
let _ =
  Run.script_cpp (fun _ ->
    Loop.elim_loop_single [cFor "i"]
  )
