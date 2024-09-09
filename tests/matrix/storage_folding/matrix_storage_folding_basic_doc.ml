open Optitrust
open Prelude
open Target

let _ = Run.script_cpp (fun _ ->
  let a = find_var "a" [] in
  !! Matrix_basic.storage_folding ~var:a ~dim:0 ~size:(trm_int 3) [cFunBody "main"];
)
