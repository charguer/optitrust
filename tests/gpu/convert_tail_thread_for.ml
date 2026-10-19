open Optitrust
open Prelude

let _ = Flags.typechecking_mode := Flags.ProofPreserving
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
  !! Gpu.convert_tail_thread_for [] [cFunDef "single"; cFor "i"];
  !! Gpu.convert_tail_thread_for [1] [cFunDef "nested"; cFor "j"];
  !! Gpu.convert_tail_thread_for [0; 1] [cFunDef "nested_seq"; cFor "j"];
  !! Resources.ensure_computed ();
)
