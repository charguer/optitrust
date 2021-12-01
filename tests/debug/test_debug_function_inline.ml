open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 


    !! Function_basic.inline [cFun "vect_mul"];
    !!! ();

)