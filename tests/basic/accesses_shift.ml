open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.scale (trm_double 6.0) [cCellAccess ~base:[cVar "t"] [cVar "i"]];
)
