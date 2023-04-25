open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  
  !! Matrix_basic.intro_malloc [nbMulti; cFun "malloc"];

)

"
#include <stdlib.h>
#include \"../../include/optitrust.h\"
int main () {
  const int N = 10;
  int* p = (int*) malloc (N * sizeof(int));
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->
  !! Matrix_basic.intro_malloc [cVarDef "p"; cFun "malloc"];
  !! Matrix_basic.intro_malloc0 "x" [cFunDef "main"; dBody];
  !! Matrix_basic.intro_malloc0 "y" [cLabel "y_seq"];
  show [cFun ~args:[[cVar "y"]] "free"];
  !!! ();
)