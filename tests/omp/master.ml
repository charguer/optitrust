open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.for_ [Private ["i"]] [tBefore; occIndex ~nb:2 0; cFor_c "i"];
  !! Omp.single [] [tBefore; cSeq ~args:[[sInstr "toobig = 0"]] ()];
  !! Omp.for_ [Private ["i"; "y"; "error"]; Reduction (Plus, ["toobig"])] [tBefore; occIndex ~nb:2 1; cFor_c "i"];
  !! Omp.master [tBefore;cSeq ~args_pred:(Target.target_list_one_st [sInstr "++c"]) ()];
)