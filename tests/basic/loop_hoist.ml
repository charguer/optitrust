open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.hoist [cVarDef "x"];
  )
"
int main() {
  for (int i = 0; (i < 4); i++) {
    int x;
    x = (2 * i);
  }
}
"

let _ = Run.script_cpp (fun () ->

  !! Loop_basic.hoist [cVarDef "x"];
  !! Loop_basic.hoist [cVarDef "z"];
)
