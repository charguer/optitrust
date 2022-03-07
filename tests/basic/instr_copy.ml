open Optitrust
open Target



let _ = Run.doc_script_cpp (fun _ ->

  !! Instr_basic.copy [cWriteVar "x"];

)

"
int main(){
  int x = 10;
  x = 20;
}


"

let _ = Run.script_cpp (fun _ ->

  !! Instr.copy [cWriteVar "x"];
  !! Instr.copy [cWriteVar "y"];
  !! Instr.copy [cFun "f"];
)