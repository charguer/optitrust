open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.single [CopyPrivate ["lock_ptr"]] [tAfter;cVarDef "lock_ptr";];
)