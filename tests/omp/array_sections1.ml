open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target_data [Map_c (No_map,["A[0:4]"])] [tAfter; cVarDef "A"];
  !! Omp.target [Map_c (No_map, ["A[7:20]"])] [tBefore; cSeq ~args:[[sInstr "A[2] = 0"]] ()];
)
