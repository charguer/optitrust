
open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.fold ~at:[cVarDef "q"] [cVarDef "p"];
  !! Variable_basic.fold ~at:[cVarDef "x"] ~as_reference:true [cVarDef "p"];
  )
"
int main() {
  int t[2];
  int *p = &t[0];
  int* q = &t[0];
  int x = t[0];
}
"

(* TODO: rename ~as_reference to ~deref *)


let _ = Run.script_cpp (fun _->
  (* Fold everywhere *)
  !! Variable_basic.fold ~as_reference:true [cVarDef "a"];
  (* Fold at one place *)
  !! Variable_basic.fold ~at:[cVarDef "r1"] ~as_reference:true [cVarDef "y"];
  (* For at one place, then another one *)
  !! Variable_basic.fold ~at:[cVarDef "r3"] ~as_reference:true [cVarDef "b"];
  !! Variable_basic.fold ~at:[sInstr "= 9"] ~as_reference:true [cVarDef "b"];
  !! Variable_basic.fold [cVarDef "v"];
)
