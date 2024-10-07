open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Instr.move ~dest:[cFunBody "main"; tFirst] [cVarDef "c"];
  (* Using ~dest:[tBefore; cIf()] cannot work because there is a let node that is not shown in C syntax *)
  )
