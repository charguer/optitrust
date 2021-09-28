open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Variable.insert_and_fold "s1" "int" "x*y" [tAfter;cVarDef "y"];
  !! Variable.insert_and_fold "s2" "int" "y*x" [tAfter;cVarDef "r1"];
  !! Variable.insert_and_fold "s3" "int" "f(2,2)" [tAfter;cVarDef "r2"];

)
