open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.single [occIndices [0;1]; cCall "printf"];
  !! Omp.single ~clause:[Nowait] [occIndex 2; cCall "printf"];
  !! Omp.parallel [cSeq ~args_pred:(target_list_one_st [cCall "printf"]) ()];

)
