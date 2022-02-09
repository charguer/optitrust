open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.inline [cVarDef "x"];
  )
"
int main() {
  int z = 3;
  int& x = z;
  int y = x;
  y++;
}
"

let _ = Run.script_cpp ( fun _ ->
  (* unfold (without deletion of the definition) *)
  !! Variable_basic.unfold ~at:[cVarDef "r1"] [cVarDef "y"];
  !! Variable_basic.unfold ~at:[cVarDef "r3"] [cVarDef "b"];
  (* inline (with definition of the definition) *)
  !! Variable_basic.inline [cVarDef "a"];
)
