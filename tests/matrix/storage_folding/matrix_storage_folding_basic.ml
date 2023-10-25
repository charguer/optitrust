open Optitrust
open Prelude
open Target

let _ = Run.script_cpp (fun _ ->
  (* TODO LATER: what about record field x.m:
      & a = x.m
      deal with alloc assign without let

      COMBI target alloc instead of ~var
       + may introduce variable for basic
     *)
  let a = find_var_in_current_ast "a" in
  let b = find_var_in_current_ast "b" in
  !! Matrix_basic.storage_folding ~var:a ~dim:0 ~size:(trm_int 3) [cFunBody "main"];
  !! Matrix_basic.storage_folding ~var:b ~dim:0 ~size:(trm_int 3) [cFunBody "main"];
  (* TODO?
  Array.to_variables
   + generalize to Matrix.to_variables on last dim
  !! Matrix_basic.storage_folding ~storage:Variables
  + unroll and avoid rotations
    - either circular buffer + unroll + simpl;
    - or
  //
  !! Loop.rotate_values ~i
  + prelude + loop
  *)
)
