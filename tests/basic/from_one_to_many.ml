open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Generic_basic.from_one_to_many ["x1";"x2"] [cVarDef "x"];
)

(* TODO: ARTHUR: this should be obtained as a combi transformation
    using delocalize + array_to_variables *)