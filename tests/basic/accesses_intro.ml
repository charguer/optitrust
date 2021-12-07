open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Function_basic.inline ~body_mark:"body" [nbMulti; cFun "foo"];
    !! Accesses_basic.intro [nbMulti; cMark "body"];


    (* LATER ARTHUR: this code does nothing, it seems ok
    !! Accesses_basic.intro [nbMulti; cVarDef "e"];
    !! Accesses_basic.intro [nbMulti; cVarDef "f"]; *)
    (* note that g was simplified at conversion time *)
)