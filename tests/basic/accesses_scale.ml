open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.scale (trm_double 5.0) [cOr [[cCellWrite ~base:[cVar "t"] ~index:[cVar "i"]];[cCellRead ~base:[cVar "t"] ~index:[cVar "i"]]]]
)
