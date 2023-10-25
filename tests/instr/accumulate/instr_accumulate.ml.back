open Optitrust
open Target

let _ = Run.doc_script_cpp (fun () -> 

  !! Instr.accumulate ~nb:2 [occFirst; cWriteVar "x"];

)
"
int main (){
  int x = 10;
  x = x + 1;
  x = x + 2;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Instr.accumulate ~nb:6 [occFirst; cWriteVar "x"];
  !! Instr.accumulate ~nb:6 [occFirst; sInstr "result.x +="];

)
