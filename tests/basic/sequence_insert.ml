open Optitrust
open Target

let _ =
  Run.script_cpp (fun _ ->
    !! Sequence.insert "int a = 5; const float b = 5.0;" [tBefore; cVarDef "x"];
    !! Sequence.insert "y++;" [tAfter; cVarDef "y"];
    !! Sequence.insert "p++;" [tAfter; cVarDef "y"];
    !! Sequence.insert "printf(\"%d\", y);" [tAfter; cVarDef "y"];
    !! Sequence.insert "for (int i = 0; i < 5; i++) { x++; }" [tAfter; sInstr "y++"];
    !! Sequence.insert "typedef struct { int x; int y; } vect;" [tBefore; cTopFun "main"];
    !! Sequence.insert "typedef vect myvect;" [tAfter; cTypDef "vect"];
  )