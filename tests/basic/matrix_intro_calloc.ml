open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    show [cFun "calloc"];
    !! Matrix_basic.intro_calloc [nbMulti; cFun "calloc"];

)