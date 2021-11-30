open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.interchange [cFor "i"];
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

  !! Loop_basic.interchange [cFor "a"];
  !! Loop_basic.interchange [cFor "a"];
  !! Loop_basic.interchange [cFor "c"];
  !! Loop_basic.interchange [cFor "b"];
)

(* LATER: why not call this loop.swap? *)
