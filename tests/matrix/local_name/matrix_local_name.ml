open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  let a = find_var_in_current_ast "a" in
  let b = find_var_in_current_ast "b" in
  !! Matrix_basic.local_name a ~into:"x" [cFor "i"];
  !! Matrix_basic.local_name b ~into:"y" ~alloc_instr:[cWriteVar "b"] [cFor "j"];

)
