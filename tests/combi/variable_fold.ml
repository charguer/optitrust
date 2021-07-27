open Optitrust
open Target

let _ = Run.script_cpp( fun _ ->
  !! Variable.fold ~at:[cVarDef "r1"] [cVarDef "s1" ];
  !! Variable.fold ~nonconst:true [cVarDef "s2" ];
  !! Tools.failure_expected (fun () ->
        Variable.fold [cVarDef "a"])
  (* DONE:
     at the basic level,
        Variable.fold_basic  works for both const and non-const
     at a high level:
     - Variable.fold  works for const, fails for nonconst
     - Variable.fold ~nonconst:true    : to force it working for nonconst
   *)
)