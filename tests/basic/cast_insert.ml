open Optitrust

open Target

let _ = Run.script_cpp (fun _ -> 

    Cast_basic.insert Ast.(typ_float ()) [cVar "a"];
    Cast_basic.insert Ast.(typ_double ()) [cVar "b"];

)