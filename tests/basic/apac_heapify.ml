open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->

  !! Apac_basic.heapify [cVarDef "a" ];

  !! Apac_basic.heapify [cVarDef "b" ];

  !! Apac_basic.heapify [cVarDef "c" ];

  !! Apac_basic.heapify [cVarDef "d" ];

  !! Apac_basic.heapify [cVarDef "e" ];

  !! Apac_basic.heapify [cVarDef "f" ];
  
  !! Apac_basic.heapify [cVarDef "g" ];
  
  !! Apac_basic.heapify [cVarDef "h" ];

  !! Apac_basic.heapify [cVarDef "j" ];

  !! Apac_basic.heapify [nbAny; cVarsDef "" ];

  !! Apac_basic.heapify [cVarDef "ag" ];

  !! Apac_basic.heapify [cVarDef "ah" ];

  !! Apac_basic.heapify [cVarDef "pg" ];

)
