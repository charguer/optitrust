open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! ();
  (* FIXME???
  let s = find_var_in_current_ast "s" in
  !! Matrix_basic.stack_copy ~var:s ~copy_var:"x" ~copy_dims:1 [occFirst; cFor "j"]; *)
)
