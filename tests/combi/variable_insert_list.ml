open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Variable.insert_list  ~defs:[("const int","x","1");("const int","y","x/2");("const int","z","y/x")] [tBefore; cFunDef "main"]

)