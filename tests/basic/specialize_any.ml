open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
      !! Specialize_basic.any (expr "i") [cAny];
  )
"
#include \"../../include/optitrust.h\"
int ANY(int);
int main() {
  int i = 0;
  int a = ANY(2);
}
"

let _ = Run.script_cpp (fun _ ->



  !! Specialize_basic.any (lit "2") [sInstr "corners"; cAny];

  !! Specialize_basic.any (var "i") [cAny];
)
