open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Sequence_basic.split [tAfter; sInstr "b = 0"];

)

"
int main() {
  {
    int a = 0;
    int b = 0;
    int c = 0;
    int d = 0;
  }
}
"

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.split [tAfter; cFunDef "main"; sInstr "y++"];

)
