open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

    !! Variable.insert_list ~names:["x";"y";"z"] ~values:["1";"x/2";"y/x"] ~typ:"const int" [tBefore; cFunDef "main"]

)