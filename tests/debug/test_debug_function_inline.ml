open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Function_basic.inline ~body_mark:"foobd" [nbMulti; cFun "foo"];
    !! Accesses.intro [nbMulti; cMark "foobd"];
    !!!();
    !! Function_basic.inline [cFun "vect3_mul"];
    !!! ();
)