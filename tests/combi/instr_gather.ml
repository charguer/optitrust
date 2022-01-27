open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Instr.(gather_targets ~dest:GatherAtFirst) [cVarDef ~regexp:true "a."];
  )
"
int main() {
  int a1 = 0;
  int b1 = 0;
  int a2 = 0;
  int b2 = 0;
}
"


let _ = Run.script_cpp (fun _ ->

  !! Instr.(gather_targets ~dest:(GatherAtFirst)) [cVarDef ""];
  !! Trace.alternative (fun _ ->
      Instr.(gather_targets ~dest:(GatherAtLast)) [cVarDef ""];
      !! ();
     );
  !! Trace.alternative (fun _ ->
      !! Instr.(gather_targets ~dest:(GatherAt [tBefore; cFor "i"])) [cVarDef ""];
      !! ();
     );
  !! Trace.alternative (fun _ ->
      Instr.(gather_targets ~dest:(GatherAt [tAfter; cFor "k"])) [cVarDef ""];
      !! ();
     );


)
