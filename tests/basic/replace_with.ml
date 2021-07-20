open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  (* !! Generic.replace_with_arbitrary "int y = 5" [cVarDef "x"]; *)
  (* !! Generic.replace_with_arbitrary "f(5)" [cFun "f"]; *)
)