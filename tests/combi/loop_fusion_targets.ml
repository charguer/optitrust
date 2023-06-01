open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  (*!! Instr.gather_targets ~dest:(GatherAt [tBefore; occLast; cFor "i"]) [nbMulti; cFor "i"];*)
  !! Loop.fusion_targets ~nest_of:2 ~into:[occLast; cFor "i"] [nbMulti; cFor "i"];
)

"
int main() {
  int x = 0;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j += 2) {
      x++;
    }
  }
  int y = 0;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j += 2) {
      y++;
    }
  }
}
"


let _ = Run.script_cpp ( fun _ ->
  !! Loop.fusion_targets [multi cFor ["i"; "j"]];
  Trace.step_justif "foo!!";
  !! Loop.fusion_targets ~nest_of:2 [nbMulti; cFor "k0"];
  Trace.step_justif "bar"
)
