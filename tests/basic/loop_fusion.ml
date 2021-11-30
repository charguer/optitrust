open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.fusion_on_block [cLabel "tofuse"];
  )
"
int main() {
  int x, y;
  tofuse: {
    for (int i = 0; (i < 5); i++) {
      x += i;
    }
    for (int i = 0; (i < 5); i++) {
      y += i;
    }
  }
}
"

let _ = Run.script_cpp ( fun _ ->

  (*!! Sequence_basic.intro ~mark:"tofuse" 3 [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];*)

  !! Loop_basic.fusion_on_block [cLabel "tofuse"];
)
