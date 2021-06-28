open Optitrust
open Target

let _ =
  Run.script_cpp (fun _ ->
    !! Sequence.insert [tBefore; cVarDef "x"] "int a = 5;const float b = 5.0";
    !! Sequence.insert [tAfter; cVarDef "y"] "y++;";
    !! Sequence.insert [tAfter; cVarDef "y"] "printf(\"%d\", y);";
    !! Sequence.insert [tAfter; sInstr "y++"] "for (int i = 0; i < 5; i++) { x++; }";(* This doesn't print the expression inside the for loop body' *)
    !! Sequence.insert [tBefore; cTopFun "main"] "typedef struct { int x; int y; } vect;";
    !! Sequence.insert [tAfter; cTypDef "vect"] "typedef vect myvect;";

  )