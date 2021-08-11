open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for [Shared_c ["x";"y";"index";"n"]] [tBefore;cFor_c "i"];
  !! Omp.atomic (Some Update) [tBefore; sInstr "x[index[i]] += work1(i)"];
)