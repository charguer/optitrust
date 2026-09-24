open Optitrust
open Prelude

(* let _ = Flags.save_ast_for_steps := Some Flags.Steps_all *)

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();
  !! Loop.hoist_instr ~dest:[tAfter; cFor "i"] ~down:true [cForBody "j"; dSeqNth 2];
  !! Loop.hoist_instr ~dest:[tBefore; occFirst; cFor "i"] [occFirst; cForBody "j"; dSeqNth 0];
)
