open Optitrust
open Target
open Ast

(* TODO: why is this not working? 
  I have no clue :)
let _ = Run.doc_script_cpp (fun _ ->
    !! Sequence_basic.insert (stmt "a++;") [tBefore; cVarDef "c"] [tAfter; cVarDef "a"];
  )
"
int main() {
  int a = 1;
  int c = 2;
}
"
*)

let _ =
  Run.script_cpp (fun _ ->

    !! Sequence_basic.insert (stmt "int a = 5; const float b = 5.0;") [tBefore; cVarDef "x"];
    !! Sequence_basic.insert (stmt "y++;") [tAfter; cVarDef "y"];
    !! Sequence_basic.insert (stmt "p++;") [tAfter; cVarDef "y"];
    !! Sequence_basic.insert ~reparse:true (stmt "printf(\"%d\", y);") [tAfter; cVarDef "y"];
    !! Sequence_basic.insert (stmt "for (int i = 0; i < 5; i++) { x++; }") [tAfter; sInstr "y++"];
    !! Sequence_basic.insert (stmt "typedef struct { int x; int y; } vect;") [tBefore; cTopFunDef "main"];
    !!! Sequence_basic.insert ~reparse:true (stmt "typedef vect myvect;") [tAfter; cTypDef "vect"];
    !! Sequence_basic.insert (stmt "int test () {return 0;}") [tAfter; cTypDef "vect"];

)