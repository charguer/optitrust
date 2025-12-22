open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ = Flags.recompute_resources_between_steps := false
let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun () ->
  (*!! Instr_basic.move ~dest:[tBefore; cWriteVar "a"; occFirst] [cWriteVar "a"; occLast];*)
  (*!! Gpu_basic.thread_for [cFor "i"];*)
  !! Resources.ensure_computed ();
  );
