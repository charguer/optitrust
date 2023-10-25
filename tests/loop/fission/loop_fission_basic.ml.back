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
  __ghost(ro_fork_group, \"R := &x ~> Cell, r := range(0, 5, 1)\");
  __ghost(ro_fork_group, \"R := &y ~> Cell, r := range(0, 5, 1)\");
  for (int i = 0; (i < 5); i++) {
    __reads(\"&x ~> Cell, &y ~> Cell\");
    x + 0;
    y + 0;
  }
  __ghost(ro_join_group, \"R := &x ~> Cell, r := range(0, 5, 1)\");
  __ghost(ro_join_group, \"R := &y ~> Cell, r := range(0, 5, 1)\");
}
"

let _ = Run.script_cpp ( fun _ ->
  !! Trace.failure_expected (fun () ->
    Loop_basic.fission [tBefore; sInstr "t[i] +="];
  );

  !! Loop_basic.fission [tAfter; sInstr "t[i] +="];
  !! Loop_basic.fission [tBefore; cVarDef "z"];

  !! Loop_basic.fission [tAfter; sInstr "MFREE1(5, m1);"];

)
