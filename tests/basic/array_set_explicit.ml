open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

    Arrays.set_explicit [cVarDef "t"];
)