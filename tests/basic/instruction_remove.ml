open Optitrust
open Run

(* Works *)
let _ = run_unit_test ( fun _ -> 
    Generic.remove_instructions [[cVarDef "a"];[cVarDef "v"]];
    Generic.remove_instruction [cVarDef "a"];
    Generic.remove_instruction [cVarDef "v"];
 )