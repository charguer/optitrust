open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.shift (trm_double 9.0) [cIndexSet ~base:[cVar "t"] [cVar "i"]];
)
