open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.intro_mindex (var "N") [nbMulti; cCellAccess ~base:[cVar "p"] ()];

)
