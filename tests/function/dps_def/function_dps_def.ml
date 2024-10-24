open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Function_basic.dps_def [cFunDef "test_simpl"];
  !! Function_basic.dps_def [cFunDef "test_one_branch"];
  !! Function_basic.dps_def [cFunDef "test_branches"];

  !! Function_basic.dps_def ~fn_name:"my_test_simpl" ~arg:"my_res" [cFunDef "test_simpl"];
)
