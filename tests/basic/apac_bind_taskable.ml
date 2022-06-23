open Optitrust
open Target 
open Ast
open Apac_core

let _ = Run.script_cpp (fun () -> 

  let tsk = Apac_basic.identify_taskable_functions [] in
  !! Apac_basic.bind_taskable_calls tsk [nbMulti;cFunDef "test_invariant1"];
  !! Apac_basic.bind_taskable_calls tsk [nbMulti;cFunDef "test_invariant2"];
  !! Apac_basic.bind_taskable_calls tsk [nbMulti;cFunDef "test_expression"];
  !! Apac_basic.bind_taskable_calls tsk [nbMulti;cFunDef "test_nested_call"];

)
