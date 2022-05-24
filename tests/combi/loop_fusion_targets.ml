open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.fusion_targets [cLabel "block"];
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


let _ = Run.script_cpp ( fun _ ->

  !! Loop.fusion_targets [cLabel "block"];

)
