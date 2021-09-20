open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.parallel_sections [FirstPrivate ["section_count"]] [tAfter; cVarDef "section_count"];
  !! Omp.set_num_threads 4 [tAfter;cVarDef "section_count"];
  !! Omp.set_dynamic 0  [tAfter;cVarDef "section_count"];
  !! Omp.section [tBefore; tIndex ~nb:2 0; cSeq ~args_pred:(target_list_one_st [sInstr "section_count++"])()];
  !! Omp.section [tBefore; tIndex ~nb:2 1; cSeq ~args_pred:(target_list_one_st [sInstr "section_count++"])()];
)