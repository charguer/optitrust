open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  !! Apac.heapify_nested_seq [occLast; cSeq ()];

)