open Optitrust
open Target


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
  show [cAny];
  
  !! Specialize_basic.any "i" [cAny];
)
