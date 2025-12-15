open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ = Flags.recompute_resources_between_steps := false

let _ = Run.script_cpp (fun () ->
  (*!! Instr_basic.move ~dest:[tBefore; cWriteVar "a"; occFirst] [cWriteVar "a"; occLast];*)
  !! Resources.ensure_computed ();
  !! Gpu_basic.thread_for [cFor "i"];
  !! Resources.ensure_computed ();
  );
