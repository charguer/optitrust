open Optitrust
open Target

(* let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.fission [tBefore; sInstr "y +="];
  )
"
int main() {
  int x;
  int y;

  for (int i = 0; (i < 5); i++) {
    x += i;
    y += i;
  }
}
" *)

let _ = Run.script_cpp ( fun _ ->
  
  
  !! Loop_basic.fission [tAfter; sInstr "t[i] +="];
  !! Loop_basic.fission [tAfter; cVarDef "b"];
  !! Loop_basic.fission [tBefore; sInstr "t[i] +="];
)
