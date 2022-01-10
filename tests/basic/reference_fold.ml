open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.fold [cVarDef "p"];
  )
"
int main() {
  int* t;
  int& p = t[0];
  t[0]++;
}
"

let _ = Run.script_cpp (fun _->
  (* Fold everywhere *)
  !! Variable_basic.fold [cVarDef "a"];
  (* Fold at one place *)
  !! Variable_basic.fold ~at:[cVarDef "r1"] [cVarDef "y"];
  (* For at one place, then another one *)
  !! Variable_basic.fold ~at:[cVarDef "r3"] [cVarDef "b"];
  !! Variable_basic.fold ~at:[sInstr "= 9"] [cVarDef "b"];
)
