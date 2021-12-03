open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.intro_mindex (trm_var "N") [nbMulti; cCellAccess ~base:[cVar "p"] ()];
  !! Matrix_basic.intro_mindex (trm_var "N") [cCellWrite ~base:[cVar "p"] ~index:[cTrue] (); dLHS]; (* [cCellWrite ~base:[cVar "p"] ~index:[cVar "i"] (); dLHS]; *)
)