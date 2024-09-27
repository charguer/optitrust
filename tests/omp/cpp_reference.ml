open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.task [] [tBefore; cCall "task_body"];
  !! Omp.parallel [Private ["y"]] [tFirst; cFunBody "test"];
)
