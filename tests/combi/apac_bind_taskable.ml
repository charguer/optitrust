open Optitrust
open Target 
open Ast
open Apac_core

let _ = Run.script_cpp (fun () -> 

  let tsk = Apac_basic.identify_taskable_functions [] in
  
  !! Apac.bind_taskable_calls tsk [nbMulti;cFunDef "test_invariant1"];
  !! Apac.bind_taskable_calls tsk [nbMulti;cFunDef "test_invariant2"];
  !! Apac.bind_taskable_calls tsk [nbMulti;cFunDef "test_detach"];
  !! Apac.bind_taskable_calls tsk [nbMulti;cFunDef "test_expression"];
  !! Apac.bind_taskable_calls tsk [nbMulti;cFunDef "test_nested_call"];
  !! Apac.bind_taskable_calls ~indepth:false tsk [nbMulti;cFunDef "test_not_indepth"; cFun "f"];

)
