open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () ->

  !! Instr.move_out [cVarDef "a"];

)
"
int main(){
  { 
   int a = 0;
   a = 10;
  }
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->

    !! Instr.move_out [cVarDefs ["b";"d"]];
    !! Instr.move_out [cFor "i"];

)
