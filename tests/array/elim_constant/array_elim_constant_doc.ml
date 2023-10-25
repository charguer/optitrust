open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Arrays.elim_constant [cVarDef "t"];
)
