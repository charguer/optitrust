open Optitrust
open Prelude

let _ = Run.script_cpp  (fun _ ->

  !! Sequence_basic.insert (stmt "int a = 5; const float b = 5.0;") [tBefore; cVarDef "x"];
  !! Sequence_basic.insert ~reparse:true (stmt "y++;") [tAfter; cVarDef "y"];

  !! Sequence_basic.insert (stmt "p++;") [tAfter; cVarDef "y"];
  !! Sequence_basic.insert (stmt "printf(\"%d\", y);") [tAfter; cVarDef "y"];
  !! Sequence_basic.insert (stmt "for (int i = 0; i < 5; i++) { x++; }") [tBefore; cVarDef "z"];
  !! Sequence_basic.insert ~reparse:true (stmt "typedef struct { int x; int y; } vect;") [tBefore; cTopFunDef "main"];
  !! Sequence_basic.insert (stmt "typedef vect myvect;") [tAfter; cTypDef "vect"];
  !! Sequence_basic.insert (stmt "int test () {return 0;}") [tAfter; cTypDef "vect"];

  !! Trace.restore_original();
  !! Sequence_basic.new_insert (stmt "p++;") [nbMulti; tAfter; cTopFunDef "main"; cVarDef ""];

)
