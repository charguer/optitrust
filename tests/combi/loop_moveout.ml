open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.move_out [cVarDef "s"];
)

"
int main() {
  for (int i = 0; i < 10; i++) {
    int r = i;
    int s = 3;
    int t = r + s;
  }
}
"

let _ = Run.script_cpp (fun _ ->
  !! Loop.move_out ~upto:"i" [cVarDef "x"];

  !! Trace.alternative (fun () ->
    !! Loop.move_out [cVarDef "x"];
    !! Loop.move_out [cVarDef "x"];
    !!());

  !! Loop.move_out [cVarDef "s"];
  !! Trace.failure_expected (fun () ->
    Loop.move_out [cVarDef "s"]);
)
