open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () ->
  !! Loop.fission ~nb_loops:2 [nbExact 1; tAfter; cFor "i"; cFor "j"; cWriteVar "y"];
  !! Loop.fission_all_instrs ~nb_loops:2 [cFor ~body:[cFor "k"] "i"];
)

"
int main(){
  int x, y, z;

  for (int i = 0; i < 10; i++){
    for (int j = 0; j < 10; j++){
      x = i;
      y = i;
      z = i;
    }
  }

  for (int i = 0; i < 10; i++){
    for (int k = 0; k < 10; k++){
      x = k;
      y = k;
    }
  }
}
"


let _ = Run.script_cpp ( fun _ ->
  !! Loop.fission_all_instrs [cFor ~body:[cVarDef "d"] "i"];
  !! Loop.fission_all_instrs ~nb_loops:2 [nbMulti; cFor ~body:[cVarDef "x"] "i"];
  !! Loop.fission ~nb_loops:3 [nbMulti; tAfter; cFor ~body:[cVarDef "y"] "i"; cVarDef "b"];
)
