open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.inline [cVarDef "x"];
  )
"
int main() {
  const int& x = 3;
  int y = x;
  y++;
}
"

let _ = Run.script_cpp ( fun _ ->
  (* inline at one specific occurence *)
  !! Variable_basic.unfold ~at:[cVarDef "r1"] [cVarDef "y"];
  !! Variable_basic.unfold ~at:[cVarDef "r3"] [cVarDef "b"];
  (* inline at all occurences and delete the reference definition *)
  !! Variable_basic.inline [cVarDef "a"];
  (* inline a reference to a matrix row *)
)


