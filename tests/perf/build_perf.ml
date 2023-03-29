open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->

  Omp.parallel [nbMulti; cFor ""];

)
