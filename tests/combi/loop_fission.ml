open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () ->
  (* FIXME: target not working *)
  (* show [cFor "i"; cFor "j"; dBody; cWriteVar "z"]; *)
  (*  tAfter ]; *)
  (* !! Loop.fission ~nb_loops:2 [tAfter; cFor "i"; cFor "j"; cWriteVar "z"]; *)
  !! Loop.fission_all_instrs [cFor "k"];
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

  for (int k = 0; k < 10; k++){
    x = k;
    y = k;
  }
}
"


let _ = Run.script_cpp ( fun _ ->
  show [cFor ~body:[cVarDef "d"] "i"];
  (*
  show (target_of_path Path.[Dir_seq_nth 0; Dir_body; Dir_seq_nth 2]);
  show (target_of_path Path.[Dir_seq_nth 5; Dir_body; Dir_seq_nth 0]);
  *)
  !! Loop.fission_all_instrs [cFor ~body:[cVarDef "d"] "i"];
  show [occLast; cFor ~body:[cVarDef "x"] "i"];
  show [nbMulti; tAfter; cFor ~body:[cVarDef "y"] "i"; cVarDef "b"]; 
  (* FIXME: only first target works *)
  (* TODO: nbMulti instead of occFirst/occLast *)
  !! Loop.fission_all_instrs ~nb_loops:2 [occFirst; cFor ~body:[cVarDef "x"] "i"];
  !! Loop.fission ~nb_loops:3 [occLast; tAfter; cFor ~body:[cVarDef "y"] "i"; cVarDef "b"];
)
