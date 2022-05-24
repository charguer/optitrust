open Optitrust
open Target

  let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.fission [tBefore; sInstr "y[i] ="];
    )
  "
  int main() {
    int x[5];
    int y[5];

    for (int i = 0; (i < 5); i++) {
      x[i] = i;
      y[i] = x[i] * x[i];
    }
  }
  "

let _ = Run.script_cpp ( fun _ ->


  !! Loop_basic.fission [tAfter; sInstr "t[i] +="];
)
