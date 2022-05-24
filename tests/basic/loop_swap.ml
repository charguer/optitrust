open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

    !! Loop_basic.swap [cFor "i"];

)

"
int main() {
  for (int i = 0; (i < 4); i++) {
    for (int j = 0; (i < 6); j++) {
    }
  }
}
"

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.swap [cFor "a"];
  !! Loop_basic.swap [cFor "a"];
  !! Loop_basic.swap [cFor "c"];
  !! Loop_basic.swap [cFor "b"];
)
