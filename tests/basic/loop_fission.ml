open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->

  !! Loop_basic.fission [tBefore; cFor "i"; sInstr "y"];

)

"
#include \"../../include/optitrust.h\"

int main() {
  __pure();

  int x;
  int y;
  for (int i = 0; (i < 5); i++) {
    __sequentially_reads(\"x ~> Cell, y ~> Cell\");
    x + 0;
    y + 0;
  }
}
"

let _ = Run.script_cpp ( fun _ ->
(*
  !! Trace.failure_expected (fun () ->
    Loop_basic.fission [tBefore; sInstr "t[i] +="];
  );
*)
  !! Loop_basic.fission [tAfter; sInstr "t[i] +="];
  !! Loop_basic.fission [tBefore; cVarDef "z"];

  !! Loop_basic.fission [tAfter; sInstr "MFREE1(5, m1);"];

)
