open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Typedef_basic.unfold ~at:[cVarDef "c"] [cTypDef "uchar"];

)
