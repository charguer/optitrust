open Optitrust
open Target
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  let tile offset size = (trm_int offset, trm_int size) in
  !! Matrix_basic.local_name_tile ~alloc_instr:[cVarDef "a"] ~into:"x" [tile 0 10; tile 2 8; tile 0 4] [cFor ~body:[cArrayWrite "a"] "i"];
  (* FIXME? non const =
  !! Matrix_basic.local_name_tile "b" ~into:"y" [tile 0 10; tile 2 8; tile 0 4] ~alloc_instr:[cWriteVar "b"] [cFor ~body:[cArrayWrite "b"] "j"]; *)
  !!! ();
)
