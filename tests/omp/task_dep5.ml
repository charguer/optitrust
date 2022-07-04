open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->
  !! Omp.task ~clause:[Private ["ii";"jj";"kk"];Depend [
    In [Dep_var "A[i:BS][k:BS], B[k:BS][j:BS]"]; Inout [Dep_var "C[i:BS][j:BS]"]]]  [cFor "ii"];
)