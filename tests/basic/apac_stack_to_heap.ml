open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  !! Apac.stack_to_heap [cVarDef "a" ];

  !! Apac.stack_to_heap [cVarDef "b" ];

  !! Apac.stack_to_heap [cVarDef "c" ];

  !! Apac.stack_to_heap [cVarDef "d" ];

)