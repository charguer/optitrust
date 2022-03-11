open Optitrust
open Target



let _ = Run.script_cpp (fun _ -> 

 !! Variable_basic.swap "x" "y" [cFunDef "test_equality"];
 !! Variable_basic.swap "x" "y" [cFunDef "test_function_call"];
 !! Variable_basic.swap "x" "y" [cFunDef "test_array"];

)