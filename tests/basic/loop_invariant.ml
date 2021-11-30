open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.invariant [cVarDef "x"];
  )
"
int main() {
  for (int i = 0; (i < 4); i++) {
    int x = 3;
    int y = (i + x);
  }
}
"

(* LATER: this is a "constant hoisting" which we decide to rename to "move_out" *)

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.invariant [cVarDef "x"];
  !! Loop_basic.invariant [cVarDef "x"];
  !! Loop_basic.invariant [cVarDef "s"];
  (* Note: currently, there is no check that the transformation is legitimate. E.g.:
      !! Loop_basic.invariant [cVarDef "s"]; *)
)
