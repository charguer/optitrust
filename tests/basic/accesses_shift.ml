open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Accesses.shift (trm_double 6.0) [cIndexAccess ~base:[cVar "t"] [cVar "i"]];
)
