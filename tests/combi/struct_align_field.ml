open Optitrust
open Target


let _ = Run.script_cpp (fun () -> 


   !! Struct.align_field 16 "items." [cTypDef "chunk"];

)