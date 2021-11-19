open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

    !! Variable.insert_list  ~defs:[("x","1");("y","x/2");("z","y/x")] ~typ:"const int" [tBefore; cFunDef "main"]

)