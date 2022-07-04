open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  !! Apac.constify_functions_arguments [dRoot];

)