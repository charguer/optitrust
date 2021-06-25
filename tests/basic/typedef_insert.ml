open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Typedef.insert "typedef struct {int x; int y} vect;" [tAfter;cVarDef "N"];
) 