open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Matrix.local_name_tile ~alloc_instr:[cVarDef "a"] ~into:"b" [(trm_int 3, trm_int 4)] [cFor "i"];
)
