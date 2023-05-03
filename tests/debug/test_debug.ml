open Optitrust
open Target
open Path

let _ = Flags.dump_ast_details := true
let _ = Flags.debug_stringreprs := true

let _ = Run.script_cpp (fun () ->
   !! Matrix.stack_copy ~var:"s" ~copy_var:"x" ~copy_dims:1 [occFirst; cFor "j"];
   !!! Debug_transfo.current_ast_at_target "HELLO" [ nbMulti; cArrayRead ~index:[cMindex ~d:(Some 2) ~args:[[]; []; []; [sExpr "j - 2"]] ()] "s"];
   !!! ();
)
