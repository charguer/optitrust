open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.equiv_at "int x, k, l; ==> k + x * l == l * x + k" [cVarInit "b"];
)
