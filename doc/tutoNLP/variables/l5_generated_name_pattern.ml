open Optitrust
open Prelude

(* Difficulty: Level 5
   Request: target the generated sum_temp variables
   Expected target: [nbMulti; cVarDef ~regexp:true "sum_temp_.*"]
   Accepted variant: [nbExact 2; cVarDef ~regexp:true "sum_temp_.*"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [nbMulti; cVarDef ~regexp:true "sum_temp_.*"];
)
