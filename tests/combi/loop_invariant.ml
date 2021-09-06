open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.invariant ~upto:"i" [cVarDef "x"];
  
  !! Trace.alternative (fun () ->
    !! Loop.invariant [cVarDef "x"];
    !! Loop.invariant [cVarDef "x"];    
    !!());
  
  !! Loop.invariant [cVarDef "s"];
  !! Loop.invariant [cVarDef "s"];
)

(* LATER: in the future, the transformation will be generalized to 
   move expressions out of arbitrary contexts *)
