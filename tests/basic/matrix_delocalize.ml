open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ -> 

  !! Sequence.intro ~mark:"mark" ~start:[tBefore;cVarDef "x"] ~stop:[tAfter;cFun "MFREE"] ();
  !! Matrix_basic.delocalize ~dim:(trm_var "N0") ~index:"i0"  [cMark "mark"];
)