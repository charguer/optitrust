open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Instr.move_invariant ~dest:[tBefore; cFor "j"] [cVarDef "x"];
  !! Instr.move_invariant ~dest:[tBefore; cFor "i"] [cVarDef "x"];
  (* NOTE: Use this transformation with care, it deos not check it the 
      targeted invariant is an invariant or not.
  *)
  Trace.alternative (fun _ -> 
  !! Instr.move_invariant ~dest:[tBefore; cFunDef "main"] [cVarDef "x"];
  !! ()
  );
)
