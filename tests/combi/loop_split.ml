open Optitrust
open Target

(* TODO: This is a combi tansformation, it should be performed after 
 applying loop hoist
*)
let _ = Run.script_cpp ( fun _ ->
  !! Loop.split [sInstr "u[i] += i"];
)
