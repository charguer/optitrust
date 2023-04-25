open Optitrust
open Target
open Path

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun () ->
   !! Matrix.stack_copy ~var_from:"s" ~var_to:"x" ~fixed_dims:1 [occFirst; cFor "j"];
   !!! ();
)
