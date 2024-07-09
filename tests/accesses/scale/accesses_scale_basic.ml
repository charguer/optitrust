open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.scale ~factor:(trm_float 5.0) [cCellReadOrWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];

)
