open Optitrust
open Prelude



let _ = Run.script_cpp (fun _ ->

  !! Align_basic.def (lit "16") [nbMulti; cVarDef ""];

)
