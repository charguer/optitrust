
open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.fold ~at:[cVarDef "q"] [cVarDef "p"];
  !! Variable_basic.fold ~at:[cVarDef "x"] ~deref:true [cVarDef "p"];
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
  !! Variable_basic.fold ~at:[cVarDef "r1"] ~deref:true [cVarDef "y"];
  (* Example with arrays *)
  !! Variable_basic.fold ~deref:true [cVarDef "a"];
  (* Example with matrices *)
  !! Variable_basic.fold ~at:[cVarDef "r3"] ~deref:true [cVarDef "b"];
  !! Variable_basic.fold ~at:[sInstr "= 9"] ~deref:true [cVarDef "b"];
  (* !! Variable_basic.fold ~deref:true [cVarDef "v"]; *) (* CHeck why this is not working *)
)

(* TODO: why do we need the argument ~deref? couldn't we read this information in the definition? *)
