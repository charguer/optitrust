open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.local_name  "a" ~into:"x" [cFor "i"];
  !! Matrix_basic.local_name  "b" ~into:"y" ~alloc_instr:(Some [cWriteVar "b"]) [cFor "j"];
)

