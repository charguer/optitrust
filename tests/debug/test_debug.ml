open Optitrust
open Target
open Path

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun () ->
   !! Matrix.stack_copy ~name:"s" ~stack_name:"x" ~d:1 [occFirst; cFor "j"];
   !!! ();
)
