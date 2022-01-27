open Optitrust
open Target

(* TODO:  int t = &r + (&s); displays incorrectly *)

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
  !! Loop.move_out [cVarDef "s"];
)

