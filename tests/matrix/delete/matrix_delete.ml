open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  let (a, _) = find_var "a" [] in
  let (b, _) = find_var "b" [] in
  let (c, _) = find_var "c" [] in
  !! Matrix_basic.delete ~var:a [cFunBody "simple"];
  !! Matrix_basic.delete ~var:b [cFunBody "simple"];
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Matrix_basic.delete ~var:c [cFunBody "ko"]);
)
