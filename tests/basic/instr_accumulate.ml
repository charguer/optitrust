open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Instr_basic.accumulate [cLabel "fuse"];
  )
"
int main() {
  int x = 0;
  fuse: {
    x += 1;
    x += 2;
    x += 3;
  }
}
"

let _ = Run.script_cpp (fun _ ->

  !! Instr_basic.accumulate [cLabel "test1"];
  !! Instr_basic.accumulate [cLabel "test2"];

)
