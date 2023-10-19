open Optitrust
open Target



let _ = Run.script_cpp( fun _ ->
  (* Folding a constant variable *)
  !! Variable.fold ~at:[cVarDef "r1"] [cVarDef "s1"];
  (* Folding a non-constant variable, requires a flag to force it *)
  !! Variable.fold ~nonconst:true [cVarDef "s2"];
  (* Without that flag, an error is issued *)
  !! Trace.failure_expected (fun () ->
        Variable.fold [cVarDef "a"])
)