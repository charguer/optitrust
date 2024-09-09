open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

(*
let _ = Flags.print_optitrust_syntax := true
let _ = Flags.debug_errors_msg_embedded_in_ast := true
*)

let _ = Run.script_cpp (fun () ->
  (* !! Resources.ensure_computed (); *)
  let s = find_var "s" [] in
  !! Matrix_basic.stack_copy ~var:s ~copy_var:"x" ~copy_dims:1 [cFor ~body:[sInstr "+="] "j"];
  (* !!! (); FIXME: encode/decode *)
)
