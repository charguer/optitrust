open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () -> 

  !! Matrix.intro_calloc [cVarDef "p"];

)

"
#include <stdlib.h>
#include \"../../include/optitrust.h\"

int main(){
  const int N = 10;
  int* p = (int*) calloc (N, sizeof(int));
}
"

let _ = Run.script_cpp (fun _ ->

    !! Matrix.intro_calloc [cVarDef "p"];
    !! Matrix.intro_calloc [cVarDef "q"];

)
