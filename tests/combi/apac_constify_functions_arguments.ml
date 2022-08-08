open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  let c = Apac.identify_constifiable_functions [] in
  
  !! Apac.constify_functions_arguments c [cFunDefAndDecl ""];

)