open Optitrust
open Target

(* TODO: Fix the issue when using labels instead of marks*)
(* let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.fusion_targets [cLabelBody "block"];
  )
"
int main() {
  block: {
    int x = 0;
    for (int i = 0; i <3; i++) {
      x++;
    }
    int y = 0;
    for (int i = 0; i < 3; i++) {
      y++;
    }
  }
}
"
 *)

let _ = Run.script_cpp ( fun _ ->

  !! Sequence_basic.intro ~mark:"tofusion" 8 [cFor "i" ~body:[sInstr "t[i]"]];

  !! Loop.fusion_targets [cMark "tofusion"];
)
