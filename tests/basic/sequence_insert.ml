open Optitrust
open Target

let _ =
  Run.script_cpp (fun _ ->
    !! Sequence_basic.insert "int a = 5; const float b = 5.0;" [tBefore; cVarDef "x"];
    !! Sequence_basic.insert "y++;" [tAfter; cVarDef "y"];
    !! Sequence_basic.insert "p++;" [tAfter; cVarDef "y"];
    !! Sequence_basic.insert "printf(\"%d\", y);" [tAfter; cVarDef "y"];
    !! Sequence_basic.insert "for (int i = 0; i < 5; i++) { x++; }" [tAfter; sInstr "y++"];
    !! Sequence_basic.insert "typedef struct { int x; int y; } vect;" [tBefore; cTopFunDef "main"];
    !! Sequence_basic.insert "typedef vect myvect;" [tAfter; cTypDef "vect"];
  )