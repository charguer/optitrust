open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  ()(* TODO *)
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
  !! Stencil.fusion_targets ~nest_of:2 ~outputs:["out"] [nbMulti; cFunBody "add2"; cFun "add"];
)
