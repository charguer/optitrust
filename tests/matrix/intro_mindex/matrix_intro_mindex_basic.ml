open Optitrust
open Target
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.intro_mindex (var "N") [nbMulti; cCellAccess ~base:[cVar "p"] ()];
  !! Matrix_basic.intro_mindex (var "N") [cCellWrite ~base:[cVar "p"] ~index:[cTrue] (); dLHS]; (* [cCellWrite ~base:[cVar "p"] ~index:[cVar "i"] (); dLHS]; *)

)
