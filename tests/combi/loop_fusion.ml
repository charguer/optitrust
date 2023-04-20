open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Loop.fusion ~nb_loops:2 [occFirst; cFor "i"]

)

"
int main() {
  int s;
  int t = 0;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j++) {
      s += i;
    }
  }
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j++) {
      t += i;
    }
  }
}
"

let _ = Run.script_cpp ( fun _ ->
  (* fuse a given number of loops *)
  !! Loop.fusion ~nb:3 [cFunDef "fusion_on_block"; occIndex ~nb:3 0; cFor "i"];

  (* fuse two loops when targeting the first one *)
  !! Loop.fusion ~nb_loops:2 [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];
)
