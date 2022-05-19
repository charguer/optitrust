open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ -> 

  !! Instr.move_out_of_fun [cVarDef "x"];

)
"
int main(){
  int x = 10;
  x = 20;
  return 0;
}
"

let _ = Run.script_cpp (fun _ -> 

  !! Instr.move_out_of_fun [cVarDef "x"];
  !! Instr.move_out_of_fun [cVarDefs ["y"; "z"]];

)
