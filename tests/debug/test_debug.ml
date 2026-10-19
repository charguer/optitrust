open Optitrust
open Prelude

let _ = Flags.typechecking_mode := Flags.ProofPreserving
let _ = Flags.disable_stringreprs := true

let _ = Run.script_cpp (fun () ->
  !! Gpu.convert_tail_thread_for [] [cFunDef "f"; cFor "i"];
  !! Resources.ensure_computed ();
)
