open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  let b = find_var_in_current_ast "b" in
  let c = find_var_in_current_ast "c" in
  !! Matrix_basic.delete ~var:a [cFunBody "simple"];
  !! Matrix_basic.delete ~var:b [cFunBody "simple"];
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Matrix_basic.delete ~var:c [cFunBody "ko"]);
)
