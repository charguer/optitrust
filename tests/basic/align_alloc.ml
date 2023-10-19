open Optitrust
open Prelude


let _ = Run.doc_script_cpp (fun () ->

  !! Align.alloc (lit "64") [cFun "MALLOC1"];

)

"
#include <stdlib.h>

#include \"../../include/optitrust.h\"

typedef int T;

int main (){

  const int N = 10;
  int *p = (int *) MALLOC1(N, sizeof(int));

  return 0;
}
"

let _ = Run.script_cpp (fun () ->

  !! Align.alloc (lit "64") [cFun "MALLOC1"];
  !! Align.alloc (lit "64") [cFun "MALLOC3"];

)