open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Variable.fold ~at:[cVarDef "r1"] [cVarDef "s1" ];
  !! Variable.fold ~nonconst:true [cVarDef "s2" ];
  !! Variable.fold ~nonconst:true ~at:[cVarDef "r"] [cVarDef "a" ];
)
