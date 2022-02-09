
open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.fold ~at:[cVarDef "q"] [cVarDef "p"];
  !! Variable_basic.fold ~at:[cVarDef "x"] [cVarDef "p"];
  )
"
int main() {
  int t[2];
  int *p = &t[0];
  int* q = &t[0];
  int x = t[0];
}
"

let _ = Run.script_cpp (fun _->
  (* Example with pointers *)
  !! Variable_basic.fold ~at:[cVarDef "r1"] [cVarDef "y"];
  (* Example with arrays *)
  !! Variable_basic.fold [cVarDef "a"];
  (* Example with matrices *)
  !! Variable_basic.fold ~at:[cVarDef "r3"] [cVarDef "b"];
  !! Variable_basic.fold ~at:[sInstr "= 9"] [cVarDef "b"];
)

