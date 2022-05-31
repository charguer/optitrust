open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () ->

  !! Instr.accumulate_targets [cWriteVar "x"];

)

"
int main() {
  int x = 0;
  x = x + 1;
  x = x + 2;
}
"


let _ = Run.script_cpp (fun _ ->

  !! Instr.accumulate_targets [cWriteVar "x"];
  !! Instr.accumulate_targets [sInstr "result.x +="];
)
