open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  
  show [nbMulti; cCellAccess ~base:[cVar "p"] ()];
  !! Matrix_basic.insert_access_dim_index (expr "N2") (expr "j") [nbMulti; cCellAccess ~base:[cVar "p"] ()] ;

)
