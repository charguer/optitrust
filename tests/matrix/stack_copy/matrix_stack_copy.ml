open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

(*
let _ = Flags.print_optitrust_syntax := true
let _ = Flags.debug_errors_msg_embedded_in_ast := true
*)

let _ = Run.script_cpp (fun () ->
  let (s, _) = find_var "s" [cFunDef "copy1from2"] in
  !! Matrix_basic.stack_copy ~var:s ~copy_var:"x" ~copy_dims:1 [cFunDef "copy1from2"; cFor "j"];

  let (s, _) = find_var "s" [cFunDef "copy1from3"] in
  !! Matrix_basic.stack_copy ~var:s ~copy_var:"x" ~copy_dims:1 [cFunDef "copy1from3"; cFor "k"];

  let (s, _) = find_var "s" [cFunDef "copy2from3"] in
  !! Matrix_basic.stack_copy ~var:s ~copy_var:"x" ~copy_dims:2 [cFunDef "copy2from3"; cFor "j"];
)
