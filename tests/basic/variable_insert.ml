open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp (fun _ ->
        Variable.insert "size" "300" [ cAfter; cTypDef "vect"];
    )