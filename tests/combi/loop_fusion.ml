open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Loop.fusion [occIndex 0; cFor "i"]

)

"
int main() {
  int s;
  int t = 0;
  for (int i = 0; i < 3; i++) {
    s += i;
  }
  for (int i = 0; i < 3; i++) {
    t += i;
  }
}
"

let _ = Run.script_cpp ( fun _ ->

  (* fuse a given number of loops *)
  !! Loop.fusion ~nb:3 [cFunDef "fusion_on_block"; occIndex ~nb:3 0; cFor "i"];

  (* fuse two loops when targeting the first one *)
  !! Loop.fusion [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];

  (* implementation details *)
  !! Trace.alternative (fun () ->
    !! Sequence_basic.intro ~mark:"tofuse" 3 [cFunDef "fusion_on_block"; cFor "i" ~body:[sInstr "t[i]"]];
    !! Loop_basic.fusion_on_block [cMark "tofuse"];
    !!());

)
