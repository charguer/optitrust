open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.for_  ~clause:[Private ["i"]] [occIndex ~nb:2 0; cFor_c ""];
  !! Omp.for_ ~clause:[Private ["i"; "y"; "error"]; Reduction (Plus, ["toobig"])] [occIndex ~nb:2 1; cFor_c ""];
 
  !! Omp.single [cSeq ~args:[[cWriteVar "toobig"]] ()];
  !! Omp.master [cSeq ~args_pred:(Target.target_list_one_st [cFun "printf"]) ()];

)