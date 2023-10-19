open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  !! Matrix_basic.delete ~var:a [cFunBody "main"];
)
