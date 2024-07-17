open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Matrix.local_name_tile
    (* ~alloc_instr:[cVarDef "a"] *)
    ~var:"a" ~local_var:"b"
    ~uninit_pre:true ~uninit_post:true
    (* [(trm_int 3, trm_int 7)] *)
    [cFor "i"];
)
