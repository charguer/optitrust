open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.set_default_device "default_device + 1" [tBefore; cIf ()];
)
 