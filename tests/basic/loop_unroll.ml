open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

  !! Loop_basic.unroll [cFor "i"];

)

"
int main() {
  int s = 0;
  for (int i = 0; (i < 3); i++) {
    s += i;
  }
}

"

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.unroll ~braces:true [cFor "i"];

)
