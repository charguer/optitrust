open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp ( fun _ -> 
    Generic.remove_instructions [[cVarDef "a"];[cVarDef "v"]];
 )
 