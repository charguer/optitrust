open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.task [If "0"] [tBefore; cSeq ~args:[cFor_c "i"] ()];
  !! Omp.task [] [tBefore;cFor_c "i"];
  !! Omp.task [] [tBefore; cFor_c "i";cFun "bar"];
  !! Omp.task [Final "1"] [tBefore; cSeq ~args:[cFor_c "j"] ()];
  !! Omp.task [] [tBefore;cFor_c "j"];
  !! Omp.task [] [tBefore; cFor_c "j";cFun "bar"];
)
