open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Instr_basic.accumulate [cLabelBody "test1"];
  !!! Instr_basic.accumulate [cLabelBody "test2"];
  

)
