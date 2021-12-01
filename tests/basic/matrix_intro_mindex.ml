open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.intro_mindex (trm_var "N") [cCellWrite ~base:[cVar "p"] ~index:[cTrue] (); dLHS]; (* [cCellWrite ~base:[cVar "p"] ~index:[cVar "i"] (); dLHS]; *)
  (* TODO: use a cAccess target with ~addr:[cVar "p"], and no ~index argument (default should be cTrue). *)
)