open Optitrust
open Syntax

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->

  !! Loop.swap [cFor "i"];

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

  !! Loop.swap [cFor "a"];
  !! Loop.swap [cFor "a"];
  !! Loop.swap [cFor "c"];
  !! Loop.swap [cFor "b"];

  !! Trace.failure_expected (fun () ->
    Loop.swap [cFor "i"]);

)
