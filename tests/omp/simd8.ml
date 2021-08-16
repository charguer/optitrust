open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.simd [LastPrivate ["pri"]] [tBefore; cFunDef "do_work"; cFor "i"];
)
