open Optitrust
open Target



let _ = Run.script_cpp (fun _ ->
    !! Variable.fold ~at:[cVarDef "b"] [cVarDef "a"];
  )
