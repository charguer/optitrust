open Optitrust
open Target
open Ast 

let _ =
  Run.script_cpp (fun _ ->
    !! Sequence_basic.insert (code "int a = 5; const float b = 5.0;") [tBefore; cVarDef "x"];
    !! Sequence_basic.insert (code "y++;") [tAfter; cVarDef "y"];
    !! Sequence_basic.insert (code "p++;") [tAfter; cVarDef "y"];
    !! Sequence_basic.insert (code "printf(\"%d\", y);") [tAfter; cVarDef "y"];
    !! Sequence_basic.insert (code "for (int i = 0; i < 5; i++) { x++; }") [tAfter; sInstr "y++"];
    !! Sequence_basic.insert (code "typedef struct { int x; int y; } vect;") [tBefore; cTopFunDef "main"];
    !! Sequence_basic.insert (code "typedef vect myvect;") [tAfter; cTypDef "vect"];
  )