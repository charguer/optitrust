open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.task [Private ["ii";"jj";"kk"];Depend (In ["A[i:BS][k:BS], B[k:BS][j:BS]"]); Depend (Inout ["C[i:BS][j:BS]"]) ] [tBefore; cFor_c "ii"];
)