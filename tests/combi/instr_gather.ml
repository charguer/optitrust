open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Instr.(gather_targets ~dest:(GatherAtFirst)) [cVarDef ""];
  !! Trace.alternative (fun _ -> 
      Instr.(gather_targets ~dest:(GatherAtLast)) [cVarDef ""];
      !! ();
     );
  !! Trace.alternative (fun _ -> 
      Instr.(gather_targets ~dest:(GatherAt [tBefore; cFor "i"])) [cVarDef ""];
      !! ();
     );
  !! Trace.alternative (fun _ -> 
      Instr.(gather_targets ~dest:(GatherAt [tAfter; cFor "k"])) [cVarDef ""];
      !! ();
     );
  
  
)
