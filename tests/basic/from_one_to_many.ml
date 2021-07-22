open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Generic.from_one_to_many ["x1";"x2"] [cVarDef "x"];
)

(* TODO: ARTHUR: write specifications *)