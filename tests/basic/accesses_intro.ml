open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

    !! Function_basic.inline ~body_mark:"foobd" [nbMulti; cFun "foo"];
    !! Accesses_basic.intro [nbMulti; cMark "foobd"];
)