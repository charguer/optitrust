open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

  !! Sequence_basic.intro_on_instr [cVarDef "b"];

)

"
int main() {
  int a = 0;
  int b = 1;
  a++;
}
"

let _ = Run.script_cpp (fun _ ->

    !! Sequence_basic.intro_on_instr [cVarDef "x"];
    !! Sequence_basic.intro_on_instr [sInstr "y = 1"];

)
