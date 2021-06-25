open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp (fun _ ->
    !!Variable.insert "a" "300" [ tAfter; cTypDef "vect"];
    !!Variable.insert "b" "300" [ tAfter; cVarDef "x"];
)