open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->

  !! Instr.(gather_targets ~dest:GatherAtFirst) [cVarDef ~regexp:true "a."];

)

"
#include \"../../../include/optitrust.h\"

int main() {
  __pure();

  int a1 = 0;
  int b1 = 0;
  int a2 = 0;
  int b2 = 0;
}
"


let _ = Run.script_cpp (fun _ ->
  !! Trace.failure_expected (fun () ->
    Instr.(gather_targets ~dest:(GatherAtLast)) [cVarDef ""]);

  !! Instr.(gather_targets ~dest:(GatherAtFirst)) [cVarDef ""];
  !! Trace.alternative (fun _ ->
      !! Instr.(gather_targets ~dest:(GatherAt [tBefore; cFor "i"])) [cVarDef ""];
      !! ();
     );
  !! Trace.failure_expected (fun _ ->
      Instr.(gather_targets ~dest:(GatherAt [tAfter; cFor "k"])) [cVarDef ""];
     );

)
