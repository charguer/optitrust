open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.task [If "0"] [tBefore; cSeq ~args:[(cFor "i"]] ()];
  !! Omp.task [] [tBefore;cFor "i"];
  !! Omp.task [] [tBefore; cFor "i";cFun "bar"];
  !! Omp.task [Final "1"] [tBefore; cSeq ~args:[[cFor "j"]] ()];
  !! Omp.task [] [tBefore;cFor "j"];
  !! Omp.task [] [tBefore; cFor "j";cFun "bar"];
)
