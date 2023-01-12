open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.single [occIndices [0;1]; cFun "printf"];
  !! Omp.single ~clause:[Nowait] [occIndex 2; cFun "printf"];
  !! Omp.parallel [cSeq ~args_pred:(target_list_one_st [cFun "printf"]) ()];

)
