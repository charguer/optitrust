open Optitrust
open Target

(* Note: currently, there is no check that the transformation is legitimate. E.g.:
      !! Loop_basic.move_out [cVarDef "s"]; *)

let _ = Run.doc_script_cpp (fun _ ->

    !! Loop_basic.move_out [cVarDef "x"];

)

"
int main() {
  for (int i = 0; (i < 4); i++) {
    int x = 3;
    int y = (i + x);
  }
}
"

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.move_out [cVarDef "x"];
  !! Loop_basic.move_out [cVarDef "x"];
  !! Loop_basic.move_out [cVarDef "s"];
  
)
