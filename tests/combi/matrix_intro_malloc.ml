open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () -> 

  !! Matrix.intro_malloc [cVarDef "p"];

)

"
#include <stdlib.h>
#include \"../../include/optitrust.h\"

int main() {
  const int N = 10;
  int* p = (int*) malloc (N * sizeof(int));
}
"

let _ = Run.script_cpp (fun _ ->

    !! Matrix.intro_malloc [cVarDef "p"];
    !! Matrix.intro_malloc [cVarDef "q"];

)
