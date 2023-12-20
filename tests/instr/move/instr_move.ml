open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* !! Instr.move_out ~dest:[tBefore; cFor "j"] [cVarDef "x"]; *)
  !! Instr.move ~dest:[tBefore; cFor "i"] [cVarDef "x"];
  (* NOTE: Use this transformation with care, it deos not check if the
      targeted invariant is an invariant or not.
  *)
  !! Trace.restore_original();
  !! Instr.move ~dest:[tBefore; cFunDef "main"] [cVarDef "x"];
)
