open Optitrust
open Target

let _ =
  Run.script_cpp (fun _ ->
    !! Sequence.insert [cBefore; cVarDef "x"] "int a = 5;const float b = 5.0";

    (* !! Sequence.insert [cAfter; cVarDef "y"] "y++;"; *)
    (* !! Sequence.insert [cAfter; cVarDef "y"] ["printf(\"%d\", y);"]; *)
    (* !! Sequence.insert [cAfter; cInstr "y++"] ["for (int i = 0; i < 5; i++) { x++; }"]; *)
    (* !! Sequence.insert [cBefore; cTopFun "main"] ["typedef struct { int x,y; } vect;"]; *)
    (* !! Sequence.insert [cAfter; cTypDef "vect"] ["typedef vect myvect;"]; *)

    (*

    !! Sequence.insert [cBefore; cVarDef "x"] ["int a = 5"; "const float b = 5.0"];

    *)




  )