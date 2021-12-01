open Optitrust
open Target

(* TODO: the unit test and the doc test should have an #include "../../include/optitrust.h"
   and not contain a definition of the ANY function
*)


let _ = Run.doc_script_cpp (fun _ ->
      !! Specialize_basic.any "i" [cAny];
  )
"
int ANY(int);
int main() {
  int i = 0;
  int a = ANY(2);
}
"


let _ = Run.script_cpp (fun _ ->

  !! Specialize_basic.any "2" [sInstr "corners"; cAny];
  !! Specialize_basic.any "i" [cFor "i"; cAny];
)
