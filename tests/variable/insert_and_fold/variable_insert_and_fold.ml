open Optitrust
open Target
open Prelude


let _ = Run.script_cpp (fun _ ->
  !! Variable.insert_and_fold ~name:"s1" ~typ:(ty "const int") ~value:(expr "x*y") [tAfter;cVarDef "y"];
  !! Variable.insert_and_fold ~name:"s2" ~typ:(ty "const int") ~value:(expr "y*x") [tAfter;cVarDef "r1"];
  !! Variable.insert_and_fold ~name:"s3" ~typ:(ty "const int") ~value:(expr "f(2,2)") [tAfter;cVarDef "r2"];

)
