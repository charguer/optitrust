open Optitrust
open Target


let _ = Run.script_cpp (fun () -> 


   !! Struct.align_field (lit "16") "items." [cTypDef "chunk"];

)